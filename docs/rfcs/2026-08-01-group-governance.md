# Democratic groups: member referenda over an authenticated membership log

## Motivation

From a member's point of view, a SimpleX group belongs to whoever created it, permanently. That person (or whoever they
promoted, or whoever ends up holding their device) can rename the group, rewrite its rules, remove any member, silence
them for everyone, and delete the whole thing. Members have no recourse except leaving: rebuilding elsewhere, losing the
history, re-inviting everyone by hand and hoping they follow. In a handful of friends this may be fine. In a
small or mid-sized community (one that has outgrown its founding circle, developed its own norms and its own reason to
exist, and would carry on if any particular person left), it means the community is a guest in someone else's space, and
everyone can feel it the first time a disagreement with an admin has no procedure attached to it.

The failure modes are not hypothetical, and this project has already hit them: "we already had several accidental
deletions or lost owner accounts" (`2024-03-14-super-peers.md`). Today an owner who loses their device takes the group's
future with them; the spec notes that if the only owner leaves, the group can no longer be deleted, and nobody can
update its profile or fix its link. A compromised owner is worse, and an owner who simply becomes hostile is worst of
all, because the software is entirely on their side. What members want here is mundane: a way for the group to outlive
any one person's device, absence, or bad behaviour, without abandoning the group and starting over.

Nathan Schneider calls this default *implicit feudalism*, [*Governable Spaces: Democratic Design for Online
Life*](https://www.ucpress.edu/books/governable-spaces/paper) (University of California Press, 2024): "a bias, both
cultural and technical, for building communities as fiefdoms", in which platforms nudge users to tolerate nearly
all-powerful admins and benevolent dictators for life. His argument is that this is a design choice rather than a
technical necessity, and that it teaches its own politics: people whose everyday online spaces are never self-governing
stop expecting self-governance anywhere. A messenger that implements only the owner role can host only fiefdoms, however
decentralised its transport. This RFC does not make groups democratic by default; many groups should stay exactly as
they are. It makes democracy *available*: an opt-in, per-group mechanism so that a community which wants to hold its
admins accountable can do so inside the app, with voice rather than only exit.

## Problem

Group admin power is absolute and unaccountable. In p2p groups any admin can remove members and demote other admins,
only owners can touch owners, and there is no recovery when owners are inactive, lost, or hostile
(`docs/protocol/simplex-chat.md`: "If the only group `owner` leaves the group, it will not be possible to delete it").
Worse, admins *are* the group infrastructure: they are the only message forwarders for not-yet-connected member pairs
(`isUserGrpFwdRelay`), the only legitimate introducers of new members, and the threat model explicitly grants them the
ability to MITM introductions, selectively drop or modify forwarded messages, and "disrupt decentralized group state by
sending different messages... to different group members" (`docs/protocol/simplex-chat.md`, threat model).

Communities that want democratic self-governance have no mechanism for it. This was named as an aspiration in
`2024-03-14-super-peers.md`: "create democratically governed communities when creators don't own the community... as the
community grows it can elect the new admins or moderators from the existing members". `2023-05-02-groups.md` concluded
that "some sort of consensus protocol is still needed for all membership changes other than member addition", and
`2024-04-01-super-peers-2.md` drafted an approval vocabulary (`MemberApproval`, `GroupConsensus`), with quorums drawn
from admins and owners only (an affected ordinary member counter-signs their own promotion, but the general membership
holds no vote).

This RFC proposes an **opt-in** governance mode for p2p groups in which the members themselves can atomically replace
the entire admin set via a referendum, with a turnout-adaptive majority rule, such that incumbent admins can neither
forge, veto, nor block the process once it has started.

Design constraints inherited from the stack:

- Deniability is a hard design goal of the messaging layer; there is deliberately no non-repudiation below the chat
  layer, so third-party-verifiable votes must be application-level signatures. This is the same path already taken for
  channel roster events (`requiresSignature` in `Protocol.hs`; design in `2025-04-14-signing-messages.md`).
- At the SMP layer, only a queue's own parties hold its keys: recipient commands (suspend, delete, key changes) require
  the recipient's key, and queue rotation is a two-party negotiation. No group member can act on another pair's queues,
  so admins have no handle on other members' direct connections. What *can* drop messages is a queue's hosting router:
  wholesale, "detectable only over other, redundant queues" (SMP threat model), which also forbids undetectably dropping
  individual messages, but each member chooses their own receiving routers, so vote censorship at that level requires
  collusion of the receiver's own chosen infrastructure, unrelated to admin power, and is mitigated by queue redundancy
  and rotation. Direct member connections are therefore the censorship-resistant substrate votes travel on.
- Interactive BFT consensus among mobile clients was assessed as impractical (`2023-10-20-group-integrity.md`: "progress
  seems unlikely or very slow"). The design below needs no interactive consensus, only asynchronous collection of
  signatures and a deterministic local tally rule.

## Solution

A group can opt in to **governed mode**. Enabling it converts all owners to admins; a governed group has no owners, and
governed clients reject any event that would create or promote a `GROwner` member, permanently. Governance parameters
are fixed by the genesis certificate; they are not part of the admin-editable group profile and cannot be changed
unilaterally.

Any member may then start a **referendum** to replace the admin set. Members vote aye/nay with Ed25519 signatures over
the proposal hash, sent to all members over their direct connections. Anyone can assemble the votes into a
**certificate**, a self-authenticating proof that the proposal passed. Every member validates the certificate
independently against its own record of members and keys and applies the new admin set atomically under a monotonic
governance version, generalizing the version-gated atomic set replacement already implemented for channel rosters
(`applyAtRosterVersion`).

The tally rule is a **simple majority of the votes cast**: the proposal passes iff `A > B`. There is no turnout quorum,
because participation thresholds reward abstention: an opponent defeats a quorumed proposal more cheaply by boycotting
it than by voting against it, which is the opposite of the behaviour a group wants. The protection against a small
minority passing a referendum unopposed is therefore not the tally rule but the *voting period* during which anyone can
cast a nay, so receivers validate the period structurally, and every member is guaranteed an objection window between
seeing a result and applying it (see "Timing"). The rule is paired with a duration that falls as support rises: a certificate
may only be evaluated after `maxReferendumDays × (1 − 2A/E)`, so weak support waits and a majority of the whole
electorate is ripe at once, immune to any outstanding vote and exempt from the challenge window (see "Duration" and
"Certificate soundness").

Incumbents cannot block a referendum in progress because: governance events are self-authenticating and accepted from
*any* member or forwarder (exempt from the single-`expectedForwarder` rule); voting rights are fixed at proposal time,
so removing or demoting voters mid-referendum does not invalidate their votes; connection teardown and send-path lockout
for removed members are deferred while a referendum is active; and certificates need no cooperation from any admin to be
assembled, transported, or applied. What incumbents can still do *before* a referendum exists is inherited from the p2p
group layer and stated under limitations.

## Design

### Scope (v1)

- p2p groups only (`useRelays = false`). Relay groups/channels are future work (see below): they are single-owner today,
  and replacing their owner set requires changes to the short-link owner chain in simplexmq. That design is deliberately
  limited: `simplexmq/rfcs/2025-04-04-short-links-for-groups.md` states its purpose "is not to comprehensively manage
  ownership changes... but rather to ensure access continuity", ranks owners so that the creator cannot be removed, and
  leaves owner-change coordination to "some simple consensus protocol between owners" that does not yet exist.
- One referendum action: replace the set of `GRAdmin` members. Moderator and member roles are untouched (manageable by
  the new admins). The action type is a sum type (`GovAction`) so profile changes, governance-parameter changes, and
  group deletion can become referendum actions later without protocol redesign.
- Fixed tally rule: simple majority of votes cast. Turnout-weighted curves and minimum-support floors are deferred (see
  open questions).

### Prerequisite: member signing keys in p2p groups

Channels already give every member a per-group Ed25519 key (`GroupMember.memberPubKey`, announced on join). The wire
format already distributes keys in p2p groups: `MemberInfo` has a `memberKey :: Maybe MemberKey` field, populated
unconditionally by `memberInfo` (`Library/Internal.hs`) from `memberPubKey`, and `MemberInfo` travels in `XGrpMemNew`,
`XGrpMemIntro` and `XGrpMemFwd`. The verification path for signed messages in p2p groups (no `GroupKeys`/
`publicGroupId`) also already exists: `withVerifiedMsg` verifies `CBGroup` signatures with prefix
`smpEncode chatBinding <> smpEncode (memberId, pubKey)` ("forward compatibility for verifying signed messages in p2p
groups", `Library/Subscriber.hs`).

What is missing for p2p groups: generating a per-group member key pair, persisting the private key (a `member_priv_key`
independent of the channels-only `GroupKeys` record), populating `memberPubKey` on join and introduction, and
TOFU-pinning received keys (rejecting a *different* key for a known member, as `applyMemberKeyRole` does for channels).
Members of existing groups announce their key on upgrade (extension of `XGrpMemInfo` or a dedicated announcement).
Governance requires keys for all electorate members; enabling fails while any current member's key is unknown.

Per-group keys also provide domain separation: a vote signature cannot be replayed in another group because both
`MemberId`s and keys are unique per group.

### Enabling governance: the genesis certificate

Preconditions: every current member's negotiated chat version supports governance (members below the version floor must
leave or be removed first; a client that silently ignores governance events would diverge), and every current member's
key is known.

**A prerequisite bug fix, outside governance.** `xGrpMemIntro` accepts the introduced member's role verbatim: unlike
`xGrpMemNew` and `xGrpMemFwd`, its p2p branch has no `checkHostRole`, so a mere admin can introduce a member carrying
role `GROwner` into a victim's local view at any time. That fake owner then satisfies every owner-gated receiver check
at that victim, including the genesis validation below, which lets an admin bootstrap governance in a group that never
opted in and with parameters of its choosing. This is a pre-existing hole with consequences beyond governance, and
governance must not be built over it. `xGrpMemIntro` must apply the same cap as its siblings, so that the introduced
role can never exceed the introducer's; with that in place an admin cannot introduce an owner at all.

Enabling requires an **authorising set** whose composition depends on whether the group still has owners:

- **Owned groups**: all current owners. The initiating owner collects signatures from all co-owners (trivial for
  sole-owner groups).
- **Ownerless groups**, where the last owner has left and no member holds `GROwner`: a strict majority of the current
  members, each signing with their member key. This is a founding certificate with the same shape as a referendum
  certificate, and it exists because the owner-signature rule is vacuous here: an empty owner set is satisfied by an
  empty signer set, so without this case any single member could enable governance unilaterally with parameters of
  their choosing. It also retires what earlier drafts deferred as future work, since an ownerless group is precisely
  the group that most needs a way to appoint admins.

In both cases the authorising set must be **non-empty**, and every signer must be a member the receiver has itself
connected to rather than merely one it has been told about, so that a minted or announced-but-unmet identity cannot
authorise the group's constitution.

The initiator then broadcasts the genesis certificate as `x.grp.gov.enable`:

- mint a random 256-bit `governanceId`, a group-scoped identifier that exists only inside e2e-encrypted messages (see
  metadata note under limitations);
- `params`: `{maxReferendumDays (default 30), challengeHours (default 24), witnessCount (default 2)}`;
- the initial electorate (list and hash, as defined in the next section);
- signed bytes: `smpEncode ("SXGG", governanceId, params, genesisLogHash)`, signed by every current owner, where
  `genesisLogHash` is the hash of the sealed initial membership log (see "Authenticated membership").

Receivers validate, fail-closed on any failure, that:

1. the group is **not already governed**. A governed group is owner-free by construction, so its owner set is empty and
   the owned-group rule would be vacuously satisfied by an unsigned certificate; without this check an attacker could
   re-genesis a governed group at will, resetting its `governanceId`, its parameters and its version chain, and
   retiring every honest reserve certificate and `prevProposalHash` anchor. A byte-identical repeat of the stored
   genesis is an idempotent no-op; a *different* genesis for a governed group is an attack, and is surfaced rather
   than applied. Governance is enabled exactly once per group;
2. the authorising set matches the rule above, is non-empty, and every signer is connected to the receiver: exactly the
   recorded `GROwner` members for an owned group, or a strict majority of the recorded current members for an
   ownerless one;
3. the parameters lie within protocol-defined bounds: `7 ≤ maxReferendumDays ≤ 30`, `24 ≤ challengeHours ≤ 168`,
   `1 ≤ witnessCount ≤ 5`. Both ends bind. An unbounded window would make every
   non-unconditional referendum unresolvable, silently reducing governance to strict-majority-of-electorate forever;
   at the other end a one-day period with a one-hour window would let two confederates replace the admin set inside a
   day, which a dormant membership routinely misses, and enabling is irreversible so the group could never undo it.
   The enabling party is exactly the actor governance exists to constrain, so its discretion over these values is
   bounded on both sides;
4. the sealed initial membership is consistent with the receiver's own member list.

It then applies atomically: store governance state, seal the initial membership as the log's genesis entry, demote all
`GROwner` members (including the sender and possibly themselves) to `GRAdmin`, and set governance version 1.

From this point the group is owner-free: no member holds `GROwner`, and governed clients additionally **reject any event
that would create or set a member with role `GROwner`**: `XGrpMemRole` to owner, and owner-role members arriving via
`XGrpMemNew`, `XGrpMemIntro`, `XGrpMemFwd`, `XGrpLinkInv` or `XGrpInv`. This is belt-and-braces alongside the
`xGrpMemIntro` fix above, which closes the same hole for groups that have not opted in. With the rule in place,
`XGrpDel` (receiver gate: sender must be `GROwner`) is dead in governed groups; nobody
can remotely delete the group, which also closes the "delete the group to pre-empt the vote" attack. Owner-gated checks
for `XGrpInfo` and `XGrpPrefs` are relaxed to `GRAdmin` in governed groups on both sides: the receiver gates and the
sender-side assertion in `runUpdateGroupProfile` (otherwise an owner-free group could never edit its profile again);
governance parameters are not carried in the profile and have no update path (v1: governance is a one-way door until a
`GAChangeGovernance` action ships; see open questions).

For newly created groups the creator enables governance at creation, a self-signed genesis over a membership of one.

Joiners receive the genesis certificate with the group, and cannot check it against a recorded owner set, because a
governed group has none. What they can check is agreement: the genesis is the root of the membership log, so its hash
is an ancestor of every frontier, and comparing frontiers with any member other than the introducing host establishes
that the joiner is on the same log as the rest of the group. This is the first point at which a host-mediated join
becomes verifiable rather than merely trusted, and it is why joiners should sync frontiers with a second peer before
participating. Until they have, they are in the position every SimpleX joiner is in today. Clients must additionally
surface governance events carrying a `governanceId` different from their stored one rather than silently dropping
them; since governance events are forwarded by everyone, a victim of a fake genesis will see mismatching traffic.

### Authenticated membership

Everything above is only as trustworthy as the membership set the votes are counted against, and in p2p groups today
that set is whatever each member's admins told them. That is not a residual social risk, it is a live protocol attack:
`xGrpMemNew` gates only on the *sender's* role, so an admin can announce fabricated members carrying keys it generated
itself, and no receiver can distinguish them from real members who have not yet connected. Announce `E + 1` phantoms,
sign their votes, and the resulting certificate is a strict majority of a majority-fabricated electorate. Selective
disclosure is as damaging without any forgery: announce one real member to some peers and not others, and no electorate
list can ever satisfy every honest receiver again. **v1 therefore requires an authenticated membership log; the
referendum machinery is not safe to ship over admin-asserted membership.**

**The log.** Each governed group has an append-only, hash-linked log of membership facts. Each entry is
`{parents :: [Hash], action, subject :: MemberId, author :: MemberId, ts, sig}` where `action` is one of `Added`
(carrying the subject's `MemberKey`), `Removed`, `RoleChanged`, or `Confirmed` (below), `parents` are the hashes of the
entries the author had when writing, and the entry's own hash covers all of it. The log is a DAG rather than a
sequence, because p2p groups have no total order; concurrent entries are siblings and are merged by taking the union.
Entries are gossiped like any other governance event, forwardable by anyone under transport rule 1, and every member
retains the whole log. It is small: its size is proportional to membership *changes*, not to messages.

**Key uniqueness.** A log entry adding a member whose `MemberKey` already appears for a different member is invalid.
Without this, one signature verifies as two votes whenever an attacker controls two member records, which costs
nothing once it controls either. Keys are already TOFU-pinned per member; the log makes them checkable across members
as well.

**Enfranchisement.** A member joins the electorate only when its `Added` entry has been countersigned by at least
`witnessCount` (default 2) distinct already-enfranchised members other than the author, each publishing a `Confirmed`
entry attesting that it completed a connection handshake with the subject. This is the load-bearing rule, and it needs
no new handshake: the mesh already emits `x.grp.mem.con` when two members connect, so `Confirmed` is that existing
signal, signed and logged. Its effect is that enfranchisement cannot be asserted, only demonstrated. A phantom that
never connects to anyone is never enfranchised no matter how many times its author announces it; to enfranchise `k`
phantoms an attacker must complete `2k` real handshakes with honest members, which is expensive, rate-limited by those
members' devices, and above all *visible to the members doing the confirming*.

This does not deliver Sybil resistance, which is unattainable without a central authority (Douceur), and an attacker
willing to run many clients and connect them to honest members can still inflate the roll. What it converts is the
attack's cost and visibility: from a silent, free, unilateral database edit into a public, attributable, interactive
one that every confirming member sees and that leaves permanent signed evidence of who vouched for whom.

**Deriving the electorate.** The electorate at a log frontier is computed deterministically: every member with an
enfranchising `Added` (per above) and no later `Removed`, regardless of role and regardless of `blockedByAdmin`,
excluding only `GRRelay`. Eligibility deliberately does not depend on role or restriction, since those are levers
incumbents control. Because the derivation is a pure function of the log, every member holding the same frontier
computes the same `E`, and the per-client `settled_at` observation that previous drafts relied on disappears, together
with its skew tolerances.

Concurrent `Added` and `Removed` for the same subject are resolved as removal-wins within a frontier, and a re-`Added`
subject is enfranchised again: this is the flickering the design already accepts elsewhere, and it is what keeps the
object at consensus number 1 (see "Related work").

**Forks are evidence.** Two honest members exchange frontiers cheaply (a list of head hashes, piggybacked on existing
traffic). If neither frontier is an ancestor of the other after fetching missing entries, the log has forked, which can
only happen if some author signed conflicting histories: an equivocation with a signed, third-party-verifiable proof
attached. Selective disclosure collapses into this case, because withholding an entry from one peer is detectable as
soon as that peer syncs frontiers with anyone else. This is what closes the pincer attack, in which one undisclosed
member made every possible electorate list invalid at someone.

**The proposal cites a frontier, not a list.** `x.grp.gov.propose` carries `logFrontier`, the sorted set of head
hashes, in place of an electorate list and hash. A receiver validates by checking that it holds every entry reachable
from that frontier, fetching what it lacks, and that the frontier is consistent with (not forked from) its own log.
Validation is therefore *repairable rather than fail-closed*: a member behind on the log catches up and proceeds, where
previously an unknown member was an unrecoverable rejection. A member that cannot reconcile the frontier with its own
log has detected a fork, and surfaces it rather than voting.

Members enfranchised after the cited frontier are not in that electorate and cannot vote in that referendum. The
guarantees hold for members enfranchised at proposal time.

**Bootstrapping and cost.** Enabling governance seals the current membership as the log's genesis entry, signed as part
of the genesis certificate; members present at that moment are enfranchised without confirmations, since their
connections predate the log. Joiners receive the log with the group and verify it by comparing frontiers with peers
other than their host, which is the first point at which a host-mediated join becomes checkable rather than merely
trusted. The `witnessCount` default of 2 is a compromise: 1 is forgeable by a single admin, and values above 2 delay
enfranchisement in sparse groups where a joiner connects slowly.

### Referendum protocol

New chat protocol events (JSON, version-gated): `x.grp.gov.enable` (above), and:

- `x.grp.gov.propose`
  `{governanceId, govVersion, action, logFrontier, prevProposalHash, proposedAt, proposer, sig}`,
  from any electorate member. `prevProposalHash` is the `proposalHash` of the referendum whose certificate the proposer
  applied at the previous version (the genesis certificate's hash at `govVersion = 2`), chaining each referendum to the
  state it was proposed against. It names the *proposal*, not the certificate, because certificates are not canonical:
  each member re-serves its own as-applied vote set (see "Catch-up and recovery"), so honest members at the same version
  would otherwise compute different chain values. `action = {type: "replaceAdmins", admins: [MemberId]}` with the
  proposed member IDs sorted, non-empty, and every named member enfranchised at the cited frontier; a receiver rejects
  the proposal otherwise, since `admins: []` would otherwise be a valid one-aye proposal leaving the group with no
  admins at all, and therefore no invitations, no moderation and no forwarding, recoverable only by another referendum
  that an attacker can answer with another abolition at every version. `proposer` is the proposer's `MemberId`, carried
  explicitly so that forwarded copies can
  be verified without trusting the forwarder's sender claim. `proposalHash` = SHA-256 of the deterministic binary
  encoding
  `smpEncode ("SXGP", governanceId, govVersion, action, logFrontier, prevProposalHash, proposedAt, proposer)`;
  `sig` is the proposer's signature over it. `govVersion` must be the receiver's stored governance version + 1 for full
  processing; a proposal claiming a higher version is not retained and only marks a possible version gap, triggering at
  most one rate-limited `x.grp.gov.request` with backoff (otherwise a single forged-version proposal would stampede the
  whole group into catch-up). Multiple proposals may coexist at the same version; clients retain up to one valid
  proposal per proposer per version (needed to validate competing certificates, bounded against floods by the electorate
  size) and members may vote on each independently, so flooding decoy proposals cannot lock anyone out of voting on the
  genuine one.
- `x.grp.gov.vote` `{governanceId, proposalHash, voter, vote, sig}`: `vote ∈ {aye, nay}`; `voter` is the voter's
  `MemberId` (for key lookup in forwarded copies; the binding is the signature itself); `sig` over
  `smpEncode ("SXGV", governanceId, proposalHash, vote)`. Sent by the voter to all members over direct connections
  (normal group fan-out). One vote per member per proposal; **conflicting signed votes from the same member on the same
  proposal annul that member's vote on it** (excluded from both tallies), a deterministic rule under vote-set union, so
  equivocating voters cannot make different members tally differently.
- `x.grp.gov.cert` `{governanceId, proposalHash, votes}`, the certificate: the full vote list
  `[(memberId, vote, sig)]`, assembled and broadcast by any member after expiry. A certificate is validated against the
  retained proposal it references; a client that lacks the proposal requests it (below) before judging the certificate.
  An **announcement form** with `votes` omitted and `{certHash, tally}` present is used for the post-apply announcement
  (see "Applying a certificate"); peers that lack the full certificate request it. At ~100 bytes per vote, groups
  beyond ~120 members need the chunked-blob transport already used for roster blobs.
- `x.grp.gov.request` `{governanceId, haveVersion, proposalHash?}`, catch-up by version, or, with `proposalHash`
  present, a request for that specific proposal + certificate; see "Catch-up and recovery".

Signatures are detached application-payload signatures over deterministic binary encodings, so they can be re-aggregated
into certificates by third parties. This is a new pattern relative to shipped message signing:
`2025-04-14-signing-messages.md` deliberately signs the transmitted bytes in an envelope to avoid re-encoding, which is
the right choice for transport authentication but cannot support third-party re-aggregation of individual votes.

Transport rules (the anti-censorship core):

1. All `x.grp.gov.*` event types are added to `isForwardedGroupMsg` and are **exempt from `expectedForwarder`** and from
   the admin-only forwarder check in `xGrpMsgForward`: they are self-authenticating, so any current member may forward
   or rebroadcast them; the existing `sharedMsgId` dedup applies. An admin dropping them achieves nothing while any
   other path exists.
2. Governance events are exempt from the `blockedByAdmin` forwarding suppression.
3. **Voting rights are fixed at proposal time.** A vote verifies against the electorate snapshot; removal, demotion, or
   blocking of the voter after the proposal does not invalidate it. Receivers keep removed-member records, so
   verification remains possible.
4. **Removal deferral**, in two tiers. (i) While a client holds an unresolved proposal, it defers the connection
   deletion normally triggered by `XGrpMemDel` (both for itself when removed and toward removed third parties) for
   members of that proposal's electorate (removals of non-electorate members proceed normally), until no live challenge
   window for the proposal can remain open: `latestClose` + the certificate freshness horizon + the maximum window (terms
   defined under "Certificate soundness" and "Applying a certificate"). (ii) A client that has only seen governance
   traffic (votes, a certificate, an announcement) *referencing* a proposal it does not hold (a member behind on
   versions must still be protected) cannot know the electorate, so it defers **all** removals, but under bounds that
   must satisfy two adversarial requirements at once: chained references must not be able to suspend removal enforcement
   indefinitely, and the budget must not be pre-consumable by an attacker so as to schedule a purge into a predictably
   unprotected gap. Recommended shape: a reference stops counting if the proposal cannot be obtained within a bounded
   fetch timeout (suggested: 24h); references are budgeted *per originating direct peer* (one peer's spam cannot
   consume protection triggered by another's traffic), with forwarded and unsigned references sharing one small separate
   budget, each capped per rolling window. Exact budgets are implementation-defined DoS tuning (open question 6). In
   both tiers the client exempts `x.grp.gov.*` events from the removed-member send guard (the `memberRemoved` check that
   otherwise blocks all sending), so a removed member can keep voting on deferred connections. Deferred connections
   carry only `x.grp.gov.*` events. This neutralizes the strongest incumbent counter-attack: mass-removing opposition
   voters to silence them mid-vote (today, receiving `XGrpMemDel` about oneself both flips the member's own status to
   removed, locking the send path, and, for non-admin members, tears down all group connections immediately).

### Tally: majority of votes cast

Let `E` = electorate size, `A` = valid aye votes, `B` = valid nay votes, `T = A + B` (turnout). The proposal passes iff

```
A > B
```

Ties fail, so the status quo wins a split vote. Abstention is neutral: not voting neither helps nor hinders a proposal,
which is why there is no turnout quorum. The rule is chosen for a property the protocol cannot supply on its own,
namely that a member can predict the outcome of their own vote without knowing the turnout. Under a turnout-weighted
curve the number of ayes required moves as votes arrive, so "how many more do we need" has no stable answer and a nay
can raise the bar for the ayes; under `A > B` each vote moves the result by exactly one, in the direction the voter
chose.

Taken alone the rule has an obvious weakness: with no nays, a single aye passes, and nothing in the arithmetic
distinguishes a genuinely unopposed proposal from one nobody noticed. The design answers that with time rather than
with a higher threshold, in the next section.

### Duration: weak support waits

A referendum has no fixed length. Instead a certificate becomes **ripe**, meaning it may be evaluated at all, only
after a delay determined by how much of the electorate has actually endorsed it:

```
ripeAt = proposedAt + maxReferendumDays × max(0, 1 − 2A/E)
```

So support buys speed and nothing else. A proposal carrying half the electorate as ayes is ripe at once; one carrying a
quarter waits half the maximum; one carrying a single vote waits almost the full 30 days. `latestClose = proposedAt +
maxReferendumDays` is the corresponding worst case, and is the term the deferral, freshness and chain rules key off.

The endpoint is not a special case bolted on: at `2A ≥ E` the delay reaches exactly zero, which is the same threshold
as the unconditional test, so "a majority of the whole electorate passes immediately" is simply where this curve meets
the axis. Above it (`2A > E`) the certificate is additionally immune to any outstanding vote, so it also skips the
challenge window. Symmetrically, a proposal is **dead** once `2B ≥ E`, since the ayes can no longer catch up even if
every remaining member votes aye; there is no reason to keep it open.

Three properties make this behave predictably.

*Only ayes move the clock.* Nays are neutral for timing and decisive only in the tally. A member who objects must never
hasten the outcome they are objecting to, and an opposition bloc must never be able to accelerate a vote by turning up.

*The clock only ever moves earlier.* `A` is monotone, so the ripeness time is monotone decreasing and every member
computes the same value from the same certificate: it is a function of the certificate's own contents, not of a live
local tally, which is what keeps it canonical across members who have seen different vote sets. The one exception is
annulment of an equivocated aye, which moves ripeness later, bounded by the cap.

*A late surge cannot snipe.* Because ripeness is per-certificate rather than a shared deadline, a bloc that votes at
the last moment produces a certificate that is ripe immediately, but the challenge window still runs from receipt, so
every member retains its objection period. Only a genuine majority of the electorate, which nothing outstanding can
overturn, skips that.

The effect on the single-aye case is the point of the design. In a group of 20 one aye ripens after 27 days, in a group
of 100 after 29, and throughout that time a single nay defeats it, since `A > B` fails at 1–1. Passing a referendum
that nobody supports now requires a month of total inattention from every other member, which is a defensible reading
of consent by silence, rather than a week of ordinary distraction. A group that wants a hard floor instead of a slow
clock can still have one; see open questions.

### Timing

There is no shared clock, so timing is enforced structurally where possible and with tolerances elsewhere:

- **Only one timestamp is on the wire.** The proposal carries `proposedAt` and nothing else; ripeness, `latestClose`
  and the freshness horizon are all derived from it and from the genesis parameters, which are fixed and signed. This
  is deliberate: an earlier draft let the proposer state its own expiry, which let a forward-dated proposal pin removal
  deferral on the whole electorate for months. A proposer can now only lie in one direction, about when it started.
- **Not-in-the-future check (always, every path):** a proposal whose `proposedAt` is more than a skew allowance
  (suggested: 24h) ahead of the receiver's clock is rejected. This needs no trust in anyone, since a local clock is
  enough to see that a referendum claims to have started tomorrow, and it is what stops a forward-dated proposal from
  pinning `latestClose`, and with it removal deferral, months into the future.
- **Plausibility check (directly received proposals only):** `proposedAt` must additionally be within the skew
  allowance of the message's broker timestamp, which is set when the message reaches the receiving queue rather than
  when the client reads it, so the check holds for offline receivers. Forwarded copies cannot be checked this way: a
  forwarder's claimed timestamp proves nothing, and late delivery is indistinguishable from backdating. Crucially, the
  sender chooses which path to use, so this check must never be the only thing standing between a backdated proposal
  and an early result. It is not; see the anchoring rule below.
- Voting stays open until the member applies or finally rejects a certificate; there is no separate voting deadline to
  miss. A member that first sees a proposal late, via a forward of a withheld one or via catch-up, votes on the same
  terms as everyone else. Late votes are counted by every member whose challenge window is still open and ignored by
  members already at local finality, which is what turns a withheld-proposal attempt into a visible contested result
  instead of silent capture (see limitations).
- **Ripeness is anchored locally as well as by the proposal:**
  `ripeAt = max(proposedAt, firstSeenAt) + delay(A)`, where `firstSeenAt` is when this member first saw the proposal.
  Taking the later of the two makes the clock robust in both directions. Backdating cannot shorten anyone's objection
  period, because a member who has just met the proposal starts its delay now regardless of what the proposal claims;
  forward-dating only postpones the proposer's own result. A member who saw the proposal on day one is unaffected,
  since `proposedAt` dominates for them. This means ripeness is a local quantity, which is correct: the guarantee it
  provides ("I get time to object") is inherently about the member holding it, and the quantities that must be
  identical everywhere, the tally and the mandate ordering, do not depend on it. Catch-up is exempt, because a stale
  certificate is judged as evidence of a settled outcome rather than raced against a clock; otherwise a member
  returning after a long absence would have to wait out a fresh delay for a referendum the group finished weeks ago.

The invariant these rules produce: every member gets at least `challengeHours` between seeing a non-unconditional
result and applying it, no matter how the proposal reached them; and a proposal cannot pass quickly without the support
that earns speed, because the delay is a function of the ayes in the certificate itself rather than of anything the
proposer asserts.

### Certificate soundness: unconditional certificates and the challenge window

Unlike a plain majority-of-electorate threshold (where omitting votes can only hurt the proposer), AQB counts nays
against the proposal, so a malicious certificate assembler would omit nay votes. Certificate validation must be robust
to selective inclusion:

- A certificate is **unconditional** if it would pass even with every electorate member not in the certificate counted
  as nay. Substituting `B ← B + (E − T)` into `A > B` gives `A > E − A`, i.e. exactly `2A > E`: ayes are a strict
  majority of the whole electorate. No withheld or future vote can flip such a certificate (only annulment of an
  equivocated aye already inside it can; see limitations), and it is applied immediately. Note this test is independent
  of the tally rule: it asks whether any outstanding vote could change the outcome, so it survives unchanged if the
  rule is later replaced.
- Any other valid certificate starts a local **challenge window** (`challengeHours`) beginning at
  `max(local first processing of the certificate, ripeAt)`. "First processing" is when this client actually decoded
  and validated it, deliberately not the broker timestamp used for freshness: anchoring the window on delivery would
  give a member who was offline for a month a window that closed before they ever saw the result, which is the
  opposite of what the window is for. The receiving member rebroadcasts the certificate, any member holding
  valid votes absent from it (in particular, nay voters themselves) resends them, and members who first saw the proposal
  late may cast *new* votes under the late-voting rule above; `x.grp.gov.vote` is idempotent, so no new event is needed.
  The window close is extension-aware and MUST be computed uniformly: the window closes `challengeHours` after the most
  recent previously-unseen valid nay for the proposal (or after the window start, if none arrived), capped at one
  `maxReferendumDays` beyond the initial close (the **maximum window**, `challengeHours + maxReferendumDays`, is thus the
  longest a window can stay open); this lets objection cohorts that learn of a result at different times aggregate
  instead of being evaluated piecemeal, and a drip-feed of nays cannot stall resolution past the cap. At window expiry
  the member evaluates the condition over the **union** of certificate votes and locally held votes, and applies iff it
  passes. Nay voters broadcast to the whole group over direct connections during the referendum period (much longer
  than the window), so in the honest-connectivity case every member already holds the nays an assembler might omit.

After a member has applied or finally rejected a certificate, further votes for that proposal are ignored (local
finality), with one exception: votes arriving inside a catch-up bundle are evaluated as part of judging that bundle (see
"Catch-up and recovery"). Members whose challenge windows saw different vote sets can diverge on a knife-edge tally; see
"Catch-up and recovery" for what is and is not repairable.

### Applying a certificate

Application is a single local transaction, generalizing `applyAtRosterVersion`:

1. check `govVersion` **greater than** the stored governance version, and (for a gap greater than one) that the
   **witnessed chain** conditions below hold. A stale version is ignored as replay, with one exception: a valid
   same-version certificate for a *different* proposal supersedes the applied one iff it ranks higher in **mandate
   order**: larger margin `A − B` first, then larger aye count `A`, then smaller `proposalHash` as the final
   deterministic tie-break. Margin leads because it is the quantity the tally rule itself decides on, and because it is
   absolute rather than relative to each proposal's own electorate: a banked certificate drawn from a small past
   electorate must not outrank a better-supported live one. All three ranking components are computed from the
   canonical certificate bytes, independent of locally held votes: local annulment governs whether a certificate is
   *acceptable*, never how it *ranks*, so the order is total and identical at every member;
2. set every member listed in the action to `GRAdmin`; demote every other current `GRAdmin` to `GRMember`;
3. store the new governance version, winning proposal and certificate;
4. **announce the applied certificate once to all connections** (unconditional certificates included) as a compact
   announcement (`proposalHash`, certificate hash, tally). The announcement is a non-authoritative hint: its tally is
   display-only, and no client acts on it without fetching and validating the full certificate; peers fetch at most once
   per unseen certificate hash, bounding traffic to O (N) full certificate transfers instead of O (N²). This is what
   makes the same-version tiebreak converge: without it, two halves of a group that applied different certificates would
   each consider themselves current and never compare (a same-version certificate yields no version gap, so no catch-up
   trigger fires).

**The witnessed chain.** Version skipping exists so that a member which could not validate some version is not stuck
forever; but an unconstrained skip would let colluders bank a reserve certificate at a version no honest referendum
will ever reach, since honest proposals advance one step at a time. For a gap greater than one, a client therefore
requires:

- a bundle for every version between its stored version and the target, each certificate passing the tally **evaluated
  on the bundle's own vote set as served, without union** (like ranking, and unlike adoption, witnessing ignores locally
  held votes; otherwise a member holding extra nays could not witness a version others applied, and the knife-edge
  rejecter's escape hatch would close);
- chain integrity: each proposal's `prevProposalHash` matching *any* valid proposal the client holds at the preceding
  version, not solely the one that won there. Accepting any same-version sibling matters because a legitimate `N+1`
  proposal authored during a contested window names whichever `N` proposal its author had applied, and rejecting it
  would strand exactly the fail-closed or knife-edge member the escape hatch exists for; the check's purpose, proving
  the author knew the preceding referendum, is unaffected. The check governs witnessed-chain links only, never gap-1
  processing or voting, where a receiver ignores `prevProposalHash` entirely. Both rules exist to keep a tolerated
  same-version race (see mandate order below) from turning a transient contested result into deadlock. Note also that
  the value is self-asserted and public (announcements broadcast the referendum's identity), so it proves only that the
  author knew the preceding referendum, not that they applied it;
- temporal consistency: each proposal's `proposedAt` after the preceding version's `latestClose`; at version 1 the anchor
  is the later of the genesis certificate's broker timestamp (already recorded as its `proposedAt` stand-in) and the
  client's own record of applying it, since genesis is not a referendum and has no expiry; and the target's `latestClose`
  in the past, since no referendum can have concluded in the future;
- a **frontier bound**: the target version may exceed by at most one the highest version attested (`SXGS`) by *several
  distinct* eligible members, outside the target certificate's aye set, and attesting through a path not controlled by
  the presenter. A single attester is not enough (one abstaining confederate is outside the aye set and can attest
  anything, the same weakness the stale-mandate limitation already concedes for corroboration), so the threshold is
  what converts fabrication from a one-member trick into a conspiracy of that size. The requirement is
  `min(3, |eligible|)` and is **never zero**: with no eligible attester reachable the certificate is not applied and
  the member stays pending. An earlier draft's "or every reachable eligible member if fewer" was fail-open in exactly
  the isolated-victim case it was meant to cover, because an attacker who has shrunk a victim's reachable set to
  nothing thereby satisfies a requirement of zero. Groups too small to field two eligible attesters cannot use the
  stale path and must wait for a live certificate or a later version.
- **for a same-version competitor**, attestations must name *that certificate's* proposal, not merely its version.
  `SXGS` binds `proposalHash`, but a bound that compares version numbers alone would let honest members' attestations
  for the legitimate certificate satisfy the check for a rival one proposed in the same round, which is precisely how a
  two-aye certificate could capture a returning member. Same-version supersede is also exempt from the temporal link
  below: that rule exists to stop a fabricated *chain* of versions, and a competitor at the version already held is not
  advancing the chain. Without the exemption the honest certificate could never supersede a rival adopted first, since
  both were proposed in the same round and neither postdates the other's close.

Conditions 1 and 2 are gap-only; the temporal link and the frontier bound apply to *every* stale adoption, gap 1
included (see "Catch-up and recovery"). The first three are cheap independent checks but are not individually
load-bearing: fabricating intermediate bundles costs only signatures (with zero nays any non-empty aye set passes the
tally (see "Tally"), and witnesses are not electorate-validated), while the timestamps in a fabricated chain are
attacker-chosen and unchecked on the catch-up path (the plausibility test of "Timing" applies only to directly received
proposals). The temporal rule bounds a leap by the time since the *group's* last completed referendum, not since the
member's own absence, so in a group that rarely holds referenda it permits a long chain; it is a cost-raiser, not the
guarantee.

What binds is the frontier bound, which anchors acceptance to state independently observed by several members with no
stake in the target certificate. Genuine long-absence catch-up is unaffected (honest peers attest the real frontier),
and a member that skipped one unvalidatable version still sees peers attesting it. The residual risk is a member whose
only reachable peers are the conspiracy: it can be shown any frontier. That is the pre-existing partition and Sybil
exposure (see limitations) rather than a new one, and it is the same reason an absolute cap on the accepted gap is worth
considering (open question 6); it bounds both this residual and the O (gap) fetch-and-store amplification a presenter
can compel.

Mandate order makes same-version arbitration substantive: a certificate can displace only one with a weaker showing, and
margins cannot be ground (they are real member signatures), so the attacker-influenced `proposalHash` decides only ties
between certificates of identical mandate strength. It still cannot elevate an unpassed proposal, though in small
groups identical margins are common rather than exceptional, so the grindable tie-break decides more often there than
the wording suggests; what it decides between is always two certificates that each passed on real votes. Known race:
actions
taken by admins of the losing certificate during the overlap are invalid in the winners' view; clients should surface a
"contested result" state, and new admins should avoid destructive actions until their certificate's window (including
extensions) has closed unchallenged.

**Certificate freshness.** The **freshness horizon** is a duration of one `maxReferendumDays` following a proposal's
`latestClose`. A certificate is fresh if it reached the receiver within the horizon, anchored like proposal timing (see
"Timing"): broker timestamp for directly received certificates (robust for offline readers), local first sight for
forwarded copies. A fresh certificate is processed as above. A stale one is only a catch-up hint: it may be adopted
solely through the corroborated catch-up path (see "Catch-up and recovery"). The same-version supersede exception in
step 1 follows the same rule (live for a fresh competitor, corroborated-catch-up-only for a stale one), which keeps
tiebreak convergence alive for members that were offline or partitioned during the race. The hard bound on withheld
certificates, however, is not freshness but step 1's **version monotonicity plus the witnessed-chain rule**: a reserve
cannot be parked above the group's reachable frontier: the witnessed chain binds acceptance to the frontier attested by
several members outside the certificate's aye set, and the `prevProposalHash` chain means any completed honest
referendum invalidates every chain pre-manufactured above it, since its identity could not have been known in advance,
so a reserve dies once the group completes any referendum at a higher version, and a reserve at the *same* version can
contest at most that one round; under mandate order it prevails only with a categorically stronger showing. Freshness
and corroboration make late enactment slow, loud, and attributable rather than impossible (see the stale-mandate
limitation).

Day-to-day admin powers are otherwise unchanged in governed groups: admins add/remove members, appoint moderators, and
may even promote additional admins between referenda; packing the admin set is pointless when a referendum can replace
the whole set at any time.

### Catch-up and recovery

A member can be behind the group's governance version: it fail-closed on an electorate conflict, finally rejected a
knife-edge certificate others applied, or was offline. Recovery (compare `XGrpRosterRequest` gap repair in the roster
machinery this design generalizes):

- A member that observes governance traffic referencing a version above its own sends
  `x.grp.gov.request {governanceId, haveVersion}`. The version is directly readable from a proposal and from signed
  membership events (in governed groups, signed `XGrpMemRole`/`XGrpMemDel` populate the existing `rosterVersion` field
  with the sender's governance version, and `XGrpMemNew` gains an equivalent optional field); a certificate or
  announcement carries no version, so for those the client first fetches the referenced proposal (bounded as in
  transport rule 4) and compares. All catch-up requests (whatever triggered them) share one per-group rate limit with
  backoff, and clients cap concurrent outstanding fetches per peer. The budget is partitioned by an observable
  criterion: fetches for references that arrived over direct member connections have reserved capacity that forwarded or
  unsigned references (announcements, votes by reference) cannot consume, with per-peer sub-budgets inside the reserve
  (admins are themselves direct peers of most members, so a single hostile direct peer must not be able to drain it), so
  reference spam cannot starve the objection path.
- Any member re-serves, per applied version above `haveVersion`, a bundle of the proposal and its **as-applied vote
  set** (the union it evaluated at finality, a superset of the broadcast certificate). To bound reflected amplification,
  a client serves a given requester only versions above what it last served them (as the roster machinery does with
  `roster_served_version`), and rate-limits serving per requester over time. The response also includes the *hashes* of
  any active proposals at the requester's new current version + 1 (full proposals are fetched on demand by
  `proposalHash`, keeping responses bounded; retention allows up to one proposal per proposer), so that a member
  advancing via catch-up regains the ability to vote in the live referendum instead of being silently disenfranchised.
- The catching-up member validates each bundle: the structural timing check, signatures, and the electorate derived from
  the bundle's cited `logFrontier` (fetching any log entries it lacks),
  skipping the live plausibility check. The tally is evaluated over the bundle's votes **union** its own locally held
  votes for that proposal, without a challenge window; the member is judging evidence of an outcome already final
  elsewhere, and *that* premise must be established, not assumed: before adopting a bundle (or a stale certificate
  routed here) that would advance or supersede its state, the client MUST apply the two load-bearing witnessed-chain
  conditions: the **temporal link** (the target's `proposedAt` after the `latestClose` of the referendum it applied at
  its stored version, and the target's `latestClose` in the past, both checkable from its own records) and the **frontier
  bound** (several distinct eligible attesters, as defined there), *regardless of gap size*. A single-version step is
  not exempt: without this, a two-member conspiracy could walk a victim forward one version at a time through this
  window-free path, indefinitely, never presenting a gap greater than one. Attestations are signed as
  `{attester :: MemberId, sig}` with `sig` over `smpEncode ("SXGS", governanceId, govVersion, proposalHash)` (keyed on
  the canonical proposal, not on a certificate, for the reason given under `prevProposalHash`), with the attester's
  `MemberId` carried for key lookup as with votes. They are served in catch-up responses and relayable by anyone other
  than the presenter and the certificate's aye-signers: relaying keeps sparsely connected members live, and excluding
  the interested parties from the relay path denies any one of them sole control of the evidence, though what actually
  makes the evidence hard to fake is the multi-signer threshold, since a confederate's attestation can always be
  laundered through an uninvolved relay. What the attestation buys is not proof of honesty but *attribution*: a
  third-party-verifiable record of who vouched for enacting the result. For a genuinely enacted certificate, nay voters
  and abstainers applied it too and can attest; for a withheld one, an attester must out itself. Both conditions are
  waived only for certificates whose aye set is a strict majority both of the proposal's electorate and of the
  receiver's electorate at its current log frontier; a banked majority that has since eroded gets no waiver, and a one-vote
  fabrication never qualifies. Falling short, the client does not apply, surfaces the pending state, and MAY offer
  user-confirmed adoption as a fallback. Version skipping means a member that cannot validate version N can still adopt
  N+1 directly, provided it holds N's bundle as a witness (step 1), served alongside N+1's in the same catch-up
  response.

What recovery can and cannot do: it repairs version lag and divergence caused by *missing information*: a bundle can
carry ayes the rejecter lacked, so a rejecter may accept on re-evaluation. It cannot force a member to accept a tally
its own held votes still contradict; such a member stays behind until a later certificate it *can* validate arrives
(which version skipping makes possible). And it is gated by connectivity: a member whose only reachable peers are the
presenter and the certificate's aye-signers (host-only joiners, never-introduced pairs) cannot corroborate a certificate
that does not qualify for the attestation waiver and stays pending, a liveness cost of the stale-mandate defense,
stated under limitations. If group membership diverges substantially in the meantime (the stranded member rejects the
new admins' membership events as unauthorized), later electorates may never validate for it; clients must surface this
state to the user rather than mask it. Bundles are self-authenticating only to the extent the receiver's own records can
verify them: for referenda predating the receiver's membership (the genesis certificate included), validation degrades
to trust in the introducing host, as with all pre-join group state.

### Why incumbents cannot prevent a referendum in progress: summary

- **Forge**: certificates require signatures from a biased majority of the electorate; admins do not hold members' keys.
- **Veto**: no admin signature or cooperation appears anywhere in propose/vote/apply, and concurrent decoy proposals
  cannot exclude anyone from voting on the genuine one (members vote per proposal, not per version).
- **Censor**: votes and certificates travel member→member over direct connections admins have no handle on (queue
  authority rests with the connection's own parties; residual router-level risk is the receiver's own chosen
  infrastructure, not the admins'; see limitations), and are forwardable by *anyone* for pairs lacking direct
  connections (transport rules 1–2).
- **Silence voters**: snapshot voting rights (rule 3) + removal deferral incl. the send-guard exemption (rule 4).
- **Rig the electorate**: the electorate is derived from the membership log at the frontier the proposal cites, so it is
  identical at every member and cannot be conjured, shrunk, or selectively disclosed without forking the log and leaving
  signed evidence; role changes and blocks never affect eligibility.
- **Rush the vote**: speed must be bought with support. A certificate cannot be evaluated before
  `maxReferendumDays × (1 − 2A/E)` has elapsed, a quantity computed from its own ayes rather than asserted by anyone,
  so a thinly supported proposal stays in the open for weeks; directly received proposals are additionally
  plausibility-checked against broker timestamps; and every member gets at least the challenge window between seeing a
  non-unconditional result and applying it.
- **Mint an owner or pre-empt by deletion**: introduced roles are capped at the introducer's, and governed clients
  reject owner-role members outright, so `XGrpDel` and other owner-gated events have no valid sender.
- **Turn governance off or re-run genesis**: parameters have no update path, and a second `x.grp.gov.enable` for an
  already-governed group is rejected rather than applied, so the constitution cannot be reset by anyone.

What incumbents *can* still do is act before a referendum exists; see limitations.

## Security analysis and limitations

- **Sybil.** Enfranchisement requires `witnessCount` confirmations from already-enfranchised members, so identities
  cannot be conjured, but they can be *grown*: an attacker who runs real clients and gets them confirmed by honest
  members accumulates real votes, and members who have long since joined ("join the same group several times... and
  pretend to be different members", threat model) are already enfranchised. This is unavoidable without a central
  authority (Douceur), so the design targets cost and visibility rather than prevention: every identity is on the log
  with a signed record of who vouched for it, which is exactly the evidence a group needs to notice and to respond with
  a referendum. `2024-03-14-super-peers.md` raises the same concern, suggesting voting power weighted by "community
  score" to "compensate for anonymous participants who could subvert the vote if plain vote count was made"; that is
  the eventual answer and is out of scope here, as it is there.
- **The genesis bootstrap is the one place local knowledge is authoritative.** Every later membership fact is checked
  against the log, but the log has to start somewhere, and its root can only be validated against what each receiver
  already believes: the recorded owner set, or for an ownerless group the recorded member list. An admin who has
  equivocated membership *before* enabling can therefore seal a skewed genesis for the members it lied to, and those
  members fail closed against everyone else rather than being silently captured. Requiring signers to be members the
  receiver has actually connected to raises the cost, and comparing frontiers with a second peer detects the result,
  but the bootstrap cannot be made self-authenticating. Enable governance in a group whose membership you can see is
  settled.
- **Pre-proposal purges.** All anti-silencing guarantees are scoped to a referendum in progress. An incumbent who moves
  first can remove suspected dissidents *before* any proposal exists, shrinking the future electorate. Removals are
  broadcast events, so a purge is visible to the remaining members, who can respond by immediately proposing (any member
  can, at any time); but members removed before that proposal are gone. The membership log makes a purge undeniable and
  attributable rather than merely visible, and removals adjacent to a proposal should be surfaced to voters, whose
  remedy is a nay; it does not make the removals reversible.
- **Withheld-proposal partition.** Colluders can distribute a proposal only among themselves and present proposal +
  certificate together at expiry. Victims receiving both via forwards accept the proposal (no timing check on forwards)
  and, under the late-voting rule, may cast fresh nays until their own challenge windows close. This defense depends on
  aggregation: a victim rejects only if the nays *it* holds at window close outnumber the ayes in the certificate (at
  `E = 100` against 40 colluding ayes, a member must hold 40 nays; one holding 39 adopts the coup, and under the
  previous turnout-weighted rule 35 would have sufficed, so the simpler tally is the weaker one here). Aggregation in
  turn depends on certificate rebroadcast and vote fan-out reaching victims within overlapping windows, which the
  window-extension rule exists to maximize, and pre-existing partitions (below) can undermine. The realistic outcome is
  therefore a contested result whose boundary tracks connectivity: early isolated evaluators may adopt, later ones
  aggregate prior nays and reject. Members offline longer than the (extended) window return at finality and their late
  nays count nowhere. An unconditional certificate cannot be produced this way without a genuine majority; a large
  colluding minority plus engineered isolation of victims is the residual risk, superseded (like all divergence here)
  by the next referendum the victims can validate.
- **Membership equivocation.** The log converts equivocation from an undetectable divergence into a fork with a signed
  proof of who authored the conflicting histories, and selective disclosure into the same case as soon as any two
  honest members compare frontiers. It does not prevent an author from equivocating, and a member whose only peers are
  the equivocator and its confederates has nobody to compare with; that residual is the pre-existing-partition case
  below.
- **Benign membership races.** Membership changes in flight when a proposal is made no longer cause rejections: the
  electorate is derived from a cited frontier, so a receiver that is behind fetches the missing entries and proceeds,
  and one that is ahead simply evaluates at the older frontier. What remains is that a member enfranchised moments
  after the frontier cannot vote in that referendum, which costs a wait rather than a partition.
- **Pre-existing partitions.** Pairs the admins never introduced (including the documented concurrent-invite race in
  `2025-11-24-member-relations-vector.md`) still cannot exchange votes directly; the any-member forwarding rule reduces,
  but does not eliminate, dependence on connectivity the incumbents shaped.
- **Router-level censorship.** A vote's delivery to member X depends on the SMP routers X chose for their receiving
  queues, which can drop a queue's messages wholesale (SMP threat model; undetectable *selective* dropping is excluded
  there, so censorship of individual votes is not available to a router). This is orthogonal to admin power, is
  mitigated by queue redundancy and rotation, and (unlike admin forwarding) is not correlated with the parties the
  referendum acts against; it is listed here because "cannot censor" claims must be scoped to the chat layer.
- **Knife-edge divergence.** Non-unconditional certificates can resolve differently across members whose challenge
  windows saw different vote sets. Unconditional certificates are exposed only through vote equivocation: a confederate
  can vote aye into the certificate and reveal a conflicting nay to selected members, annulling the aye there; if the
  certificate's margin over `E/2` is within the number of such reveals, members diverge on a certificate class that
  otherwise applies immediately. Clients may therefore hold unconditional certificates with margin ≤ a small constant
  for the challenge window as well. Version lag from such divergence is repairable via catch-up; genuine tally
  disagreement persists until a later referendum the stranded member can validate (see "Catch-up and recovery"); still
  strictly better than the status quo, where any admin state change can diverge arbitrarily, permanently, and
  undetectably.
- **Proposal spam.** Any member can propose, and proposals can coexist per version; retention is protocol-bounded at one
  proposal per proposer per version, above-version proposals are not retained at all, and clients additionally
  rate-limit per member (UI-level); a spammer can be removed by admins or voted out with their own mechanism. Decoys
  cannot lock members out of genuine votes (per-proposal voting) and cannot win without passing the tally.
- **Moderation friction from removal deferral.** While a referendum is active, removals of electorate members do not
  sever connections; at parameter ceilings the deferral can last ~120 days (~22 days at defaults: period + freshness
  horizon + window + extension cap); tier- (ii) deferral (reference-only, unscoped) is separately budgeted per
  originating peer and capped (see rule 4 and open question 6). The exposure is narrow: deferred connections carry only
  `x.grp.gov.*` events, `blockedByAdmin` still suppresses everything else, and the removal takes full effect when the
  referendum resolves, but a removed member who proposed before removal keeps a live governance channel for the
  referendum's bounded lifetime. This is the deliberate price of making mid-vote purges ineffective.
- **Stale mandates.** A passing certificate could be withheld and enacted later, including, for an unconditional
  certificate, a banked majority mandate enacted after that majority has eroded. The hard bound is version monotonicity
  together with the witnessed-chain rule: a reserve cannot be banked above the version the group can actually reach,
  since acceptance beyond a one-version gap is bounded by the frontier independently attested by several members from
  outside the certificate's aye set, and a completed referendum retires every reserve strictly below its version. A
  banked competitor at the *same* version can contest only that one round; under mandate order it prevails only by
  out-polling the live certificate on margin (equal margins fall through to aye count, then the hash), which a
  minority cannot do against a well-supported **reaffirmation referendum** (proposing the current admin set, already
  expressible as `GAReplaceAdmins`). Clients should offer reaffirmation; flushing all suspected reserves takes at most
  two successive referenda. The freshness horizon and attestation requirement do not make late enactment impossible (one
  abstaining confederate can attest, and certificates whose ayes are still a majority of the receiver's current
  membership are waived), but they make it slow, detectable, and attributable: a stale enactment arrives through the
  rate-limited catch-up path carrying a signed record of who vouched for it. The same rules govern stale same-version
  competitors, with a corroboration liveness cost for sparsely connected members (see "Catch-up and recovery").
- **Metadata.** `governanceId` is a shared random group identifier appearing only inside e2e-encrypted messages. Vote
  signatures are third-party-verifiable *within the group by design*: members can prove to each other how someone
  voted. Groups wanting ballot secrecy need a different scheme (blind/ring signatures); explicitly out of scope.

## Alternatives considered

See "Related work" at the end of this document for how these choices sit against the literature, and
`2026-08-01-group-governance-related-work.md` for the full analysis.

- **Adaptive quorum biasing** (Polkadot's positive turnout bias, `B²E < A²T`), which demands a supermajority of votes
  cast at low turnout and relaxes to majority as turnout rises. Rejected for this setting. Its required support, as a
  fraction of the electorate, is `p²/(1−p)` where `p` is the opposition share of votes cast, which is independent of
  group size: it lets a bloc win in inverse proportion to how few members object, and at any size a handful of ayes
  beats a large silent membership once opposition is thin. It is *needed* only where a majority of the electorate is
  unreachable, and *safe* only where opponents reliably vote; those hold together in token governance, which has a
  standing class of monitoring delegates, and not in a private group, where would-be objectors are drawn from the same
  attention distribution as everyone else. It also imposes a threshold that moves as votes arrive, which cannot be
  rendered honestly in a UI. Note the trade is not free: at low turnout AQB is *stricter* than `A > B`. That
  strictness is recovered on a different axis, by making weak support wait rather than clear a higher bar (see
  "Duration"), which keeps the tally rule predictable while still charging thin support a cost.
- **Fixed majority of the electorate** (`2A > E`). Retained, but as the test for applying a certificate *immediately*
  rather than as the pass condition; as a pass condition it is unreachable in groups where the transport itself
  suppresses turnout (an offline member's queue stalls at `defaultMsgQueueQuota = 128` messages, so members away from
  an active group may not receive a proposal within its period, while still counting in `E`).
- **A minimum-support floor** (`A > B` and `A ≥ max(3, ⌈E/3⌉)`), which would remove the single-aye case without
  reintroducing an abstention incentive, since a floor on ayes is unaffected by opponents staying home. Deferred to an
  open question rather than adopted, to keep v1's rule to one clause.
- **Interactive consensus (BFT / owner DAG voting)** as sketched in `2023-10-20-group-integrity.md`: rejected there
  already as impractical for mobile clients; unnecessary here since a one-shot threshold certificate suffices.
- **Approvals among privileged members only** (`2024-04-01-super-peers-2.md`): solves accidental/destructive actions
  among peers, not member sovereignty; its `MemberApproval` shape is reused here in spirit for certificate entries.
- **Aggregate signatures (BLS)** to shrink certificates, avoided: new crypto dependency; Ed25519 vote lists up to a few
  hundred members fit the existing chunked-blob transport.

## Implementation sketch

- `Protocol.hs`: `GovAction`, enable/proposal/vote/cert/request types plus the membership-log entry type and a
  `x.grp.gov.log` event carrying entries and frontier requests, six event tags (+ `isForwardedGroupMsg`); deterministic
  binary encodings with domain-separation tags (`SXGG`/`SXGP`/`SXGV`, `SXGL` for log entries, plus `SXGS` state
  attestations served in catch-up responses).
- Membership log: entry validation (parent hashes present, author enfranchised and permitted, signature), DAG merge and
  frontier computation, enfranchisement evaluation (`witnessCount` confirmations), fork detection on frontier compare,
  and derivation of the electorate at a frontier. `Confirmed` entries are emitted from the existing `x.grp.mem.con`
  path, signed; the connection event that already exists becomes the enfranchising witness.
- Key management: per-group member keypair for p2p governed groups (`group_members.member_pub_key` exists; add p2p
  private-key storage and population of `memberPubKey` on join/intro; TOFU pinning as in `applyMemberKeyRole`).
- Required, not optional, now that membership is load-bearing: sign and verify `XGrpMemNew`/`XGrpMemDel`/`XGrpMemRole`
  in governed p2p groups via the existing p2p verification branch in `withVerifiedMsg`, and mirror each into a log entry
  (independently closes the unsigned-forward forgery hole). In governed groups these events also carry the sender's
  governance version (reusing the existing `rosterVersion` field on `XGrpMemRole`/`XGrpMemDel`, plus an equivalent
  optional field on `XGrpMemNew`), which is the catch-up trigger for members with no other governance traffic.
- `Subscriber.hs`: **the `xGrpMemIntro` role cap (prerequisite, applies to all p2p groups, not only governed ones)**;
  handlers for the six events; genesis validation (already-governed rejection, non-empty authorising set, signer
  connectedness, parameter bounds on both ends); proposal validation (non-empty admin set naming enfranchised members,
  `proposedAt` not in the future on every path, broker-timestamp plausibility on direct receipt); ripeness evaluated as
  `max(proposedAt, firstSeenAt) + delay(A)` with `firstSeenAt` persisted per proposal; proposal timing validation and
  per-proposer retention cap; certificate validation + challenge-window worker with late-voting support; version-gated
  apply with witnessed-chain version skipping, the same-version mandate-order exception, and the post-apply compact
  announcement; catch-up serving from stored bundles with per-requester served-version bound and rate limiting;
  forwarder-check and `blockedByAdmin` exemptions; removal deferral incl. the governance send-guard exemption in
  `xGrpMemDel` and the send path; **rejection of owner-role members in `xGrpMemNew`/`xGrpMemIntro`/`xGrpMemFwd`/
  `xGrpMemRole` and owner-role invitations for governed groups**; relax `XGrpInfo`/`XGrpPrefs` receiver gates to
  `GRAdmin` for governed groups.
- `Commands.hs`: relax the `GROwner` assertion in `runUpdateGroupProfile` (and its callers) to `GRAdmin` for governed
  groups; new APIs below.
- Store:

```sql
ALTER TABLE groups
    ADD COLUMN governance TEXT; -- params + governanceId; null = not governed
ALTER TABLE groups
    ADD COLUMN governance_version INTEGER;
ALTER TABLE group_members
    ADD COLUMN gov_served_version INTEGER; -- catch-up amplification bound
CREATE TABLE group_membership_log
(
    entry_id     INTEGER PRIMARY KEY,
    group_id     INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
    entry_hash   BLOB    NOT NULL,
    parents      BLOB    NOT NULL, -- sorted parent hashes
    action       TEXT    NOT NULL, -- added / removed / role_changed / confirmed
    subject_id   BLOB    NOT NULL,
    subject_key  BLOB,             -- MemberKey, on `added`
    author_id    BLOB    NOT NULL,
    entry_ts     TEXT    NOT NULL,
    entry_sig    BLOB    NOT NULL,
    UNIQUE (group_id, entry_hash)
);
CREATE INDEX idx_membership_log_subject ON group_membership_log (group_id, subject_id);
CREATE TABLE group_referenda
(
    referendum_id      INTEGER PRIMARY KEY,
    group_id           INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
    proposal_hash      BLOB    NOT NULL,
    gov_version        INTEGER NOT NULL,
    action             BLOB    NOT NULL,
    log_frontier       BLOB    NOT NULL, -- cited membership-log heads; electorate derived from it
    prev_proposal_hash BLOB    NOT NULL,
    proposed_at        TEXT    NOT NULL,
    first_seen_at      TEXT    NOT NULL, -- local anchor for ripeness; not from the wire
    expires_at         TEXT    NOT NULL,
    proposer_member_id BLOB    NOT NULL,
    proposal_sig       BLOB    NOT NULL,
    status             TEXT    NOT NULL, -- active / passed / failed / superseded / witnessed (chain evidence, not applied)
    applied_cert       BLOB,             -- as-applied vote set, re-served on catch-up
    attestations       BLOB              -- third-party state attestations held for serving
);
CREATE TABLE group_referendum_votes
(
    referendum_id   INTEGER NOT NULL REFERENCES group_referenda ON DELETE CASCADE,
    group_member_id INTEGER NOT NULL,
    vote            TEXT    NOT NULL,
    vote_sig        BLOB    NOT NULL,
    annulled        INTEGER NOT NULL DEFAULT 0
);
```

- API: `APIEnableGroupGovernance` (collects owner or founding-majority signatures depending on the group's owner set),
  `APIProposeGroupAdmins`, `APIGroupVote`; certificate assembly, application, and
  catch-up are automatic. Chat items for proposal / votes / result / contested result, styled like existing group
  events.

## Future work

- **Relay groups / channels.** The same certificate machinery can gate the channel roster (replace the `== GROwner`
  checks in `xGrpRoster`/`applyAtRosterVersion` with certificate validation); but replacing *owners* there additionally
  requires threshold or majority updates to the short-link owner chain (`OwnerAuth` in the agent protocol) and
  link-queue `RKEY` authority in simplexmq, where owners are currently ranked so the creator cannot be demoted, and any
  single owner key suffices to update the link, multisig having been considered and deferred there. That is the step-2
  RFC, and it is where this design meets the project's stated roadmap item "Multisig: M-of-N approval for administrative
  actions" (`docs/protocol/channels-overview.md`). The membership log introduced here is the p2p counterpart of the
  channel roster, and the two should converge on one representation.
- **More referendum actions**: change governance parameters (including disabling), update profile/preferences, delete
  the group, replace moderators.
- **Reputation-weighted or Sybil-resistant voting**, per `2024-03-14-super-peers.md`.
- **Ballot secrecy** (blind or ring signatures) for groups that need it.

## Open questions

1. Should v1 ship `GAChangeGovernance` (at least for disabling governance) rather than making enabling a one-way door?
2. Challenge-window default: 24h assumed; mobile-offline patterns may argue for longer (matching the 7-day referendum
   period's tolerance).
3. Should the genesis certificate require unanimity of owners (current design) or majority of owners?
4. Whether to sign membership events in governed p2p groups in v1 (recommended here) or defer to a separate signing
   rollout.
5. `witnessCount` defaults to 2. Higher values raise the cost of manufacturing an enfranchised identity but delay
   enfranchisement of genuine joiners in sparse groups, and can strand a joiner whose introductions stall. Is 2 right,
   and should it scale with group size?
6. The tier- (ii) deferral and catch-up fetch budgets are two-sided DoS tuning: too tight, and purges become schedulable
   into predictably unprotected gaps; too loose, and reference spam suspends moderation or starves the objection path.
   The witnessed chain adds a third: serving and storing O (gap) bundles is itself an amplification surface, which an
   absolute cap on the accepted gap would bound along with the cost of fabricating chains, at the price of leaving very
   long-absent members to rejoin rather than catch up. All three need adversarial analysis with concrete constants
   before implementation.
7. Should certificate *acceptance* be made a pure function of the certificate, as ranking already is? It is currently
   the design's one deviation from strong eventual consistency and the direct cause of knife-edge divergence; see
   "Related work".
8. Is the support-scaled duration a sufficient answer to the single-aye case, or should the tally also carry a
   minimum-support floor (`A ≥ max(3, ⌈E/3⌉)`)? The clock makes an unsupported proposal wait roughly a month during
   which one nay defeats it, which is a strong deterrent but still resolves in favour of a lone proposer if literally
   nobody looks. A floor would make that outcome impossible, at the cost of a second clause in the rule and of
   deadlocking a group whose active population has fallen below it. The clock is preferred here because it degrades
   gracefully where a floor fails hard.

## Related work

Full analysis in [`2026-08-01-group-governance-related-work.md`](2026-08-01-group-governance-related-work.md); this is
the summary.

**Whether this is possible at all.** Under Herlihy's consensus hierarchy
([Wait-Free Synchronization](https://cs.brown.edu/~mph/Herlihy91/p124-herlihy.pdf), TOPLAS 13 (1), 1991), Frey, Gestin
and Raynal compute the synchronization power of access-control objects
([The Synchronization Power of AllowList and DenyList](https://arxiv.org/abs/2302.06344), DISC
2023, [doi:10.4230/LIPIcs.DISC.2023.39](https://doi.org/10.4230/LIPIcs.DISC.2023.39)): an AllowList has consensus number
1, a *k*-DenyList has consensus number *k*, and the entire difference is the **anti-flickering** property: once denied,
never allowed again. Promoting admins is AllowList-shaped and free; demoting them with revocation semantics would be
DenyList-shaped, and since every member verifies admin authority, *k* is the whole group, unattainable asynchronously
by [FLP](https://doi.org/10.1145/3149.214121) (Fischer, Lynch & Paterson, JACM 32 (2), 1985). **We therefore decline
anti-flickering deliberately**: a demoted admin can be re-recognized, by a later referendum and transiently during a
contested window. Revisable finality is not a compromise, it is the price of implementability, and the RFC accordingly
never claims revocation. The same shape holds for payments
in [The Consensus Number of a Cryptocurrency](https://arxiv.org/abs/1906.05574) (Guerraoui et al., PODC 2019).

**What consistency model this is.** [Byzantine Eventual Consistency](https://arxiv.org/abs/2012.00472) (Kleppmann &
Howard, 2020) characterizes the boundary by I-confluence and suggests exactly our split: aggregate I-confluently, then
decide the winner. Our vote accumulation is I-confluent (the annulment rule is defined to keep union order-independent);
the winner is decided by mandate order instead of consensus. One honest gap: strong eventual consistency
([Shapiro et al.](https://inria.hal.science/inria-00555588), 2011) requires state to be a function of received updates,
and while our *ranking* is pinned to canonical bytes, *acceptance* still unions locally held votes. That deviation is
the root of knife-edge divergence, and closing it would trade the anti-vote-withholding defence for convergence (open
question 7).

**Duelling admins.** [ERA](https://arxiv.org/abs/2601.22963) (Dougal, PaPoC
'26, [doi:10.1145/3806077.3806691](https://doi.org/10.1145/3806077.3806691)), from Element/Matrix, is the closest
published work: two admins concurrently revoking each other, where revocation is non-monotonic and forces rollbacks. Its
critique of Kleppmann's *seniority ranking* (a junior can never revoke a senior, and a revoked admin can backdate to
fake concurrency and undo their own demotion) applies directly to SimpleX's existing `roleRequiredToChange`, and is an
argument for this RFC. **We remove the duel rather than arbitrate it**: authority comes from a majority certificate
rather than from another admin, and the whole set is replaced atomically, so there is no revocation cycle. **We reject
its finality arbiter**: a mutually trusted ordering peer is the chokepoint this design exists to remove, and its
fallback of a "Creator" arbiter is unacceptable when the creator may be who the group needs to remove. Consequently we
do not get ERA's bounded total order. Its backdating analysis also confirms that our witnessed-chain temporal rules
raise cost rather than establish a bound.

**Practice in group messaging.** [MLS](https://www.rfc-editor.org/rfc/rfc9420.html) (RFC 9420) advances linear epochs
and lets the Delivery Service serialize concurrent commits, unavailable to us by construction, which is why we need
mandate order where MLS needs only a server. [DCGKA](https://doi.org/10.1145/3460120.3484542) (Weidner, Kleppmann,
Hugenroth & Beresford, CCS 2021; [eprint 2020/1281](https://eprint.iacr.org/2020/1281)) is serverless and explicitly
tolerates the same flickering: "a user may be removed and re-added, possibly indirectly (e.g., due to a remove message
'undoing' a concurrent remove)", and scopes authorization policy out, which is the gap this RFC
fills. [More is Less](https://eprint.iacr.org/2017/713) (Rösler, Mainka & Schwenk, EuroS&P 2018) found group management
messages unauthenticated in deployed messengers, which is the empirical case for signing membership events here.

**The voting rule.** We use majority of votes cast. May's theorem characterises simple majority as the unique rule that
is anonymous, neutral and positively responsive, so departures from it need a justification specific to the setting.
**We reject a turnout quorum** because participation thresholds reward abstention (opponents defeat a proposal more
cheaply by boycotting than by voting nay), and on-chain governance shows turnout far too low for fixed quorums to be
met in any case ([Feichtinger et al.](https://arxiv.org/abs/2302.12125); [Fritsch et
al.](https://arxiv.org/abs/2204.01176)). **We also reject [Polkadot's](https://arxiv.org/abs/2005.13456) adaptive
quorum biasing** (Burdges et al.: "in case of low turnout we favour the nay side, or status quo, by requiring a
super-majority approval"), despite its appeal, because its required support as a fraction of the electorate is
`p²/(1−p)` in the opposition share `p` and therefore independent of group size: it is scale-invariant in exactly the
way that makes it dangerous here, letting a small bloc win whenever opposition is thin. Its safety rests on a standing
class of attentive opponents, which token governance has and a private group does not; the DAO turnout evidence often
cited in its favour describes a population unlike this one and does not transfer. The cost of dropping it is real and
recorded in the limitations: at low turnout AQB was the stricter rule. **We reject ballot secrecy** ([Juels, Catalano &
Jakobsson](https://doi.org/10.1145/1102199.1102213), WPES 2005) because verifiable signatures are what make a
certificate self-authenticating and able to travel past a hostile forwarder, at the cost, acknowledged in the
limitations, that incumbents can identify nay voters.

**Sybil resistance and accountability.** [The Sybil Attack](https://doi.org/10.1007/3-540-45748-8_24) (Douceur, IPTPS
2002) states the limit we inherit: without a central authority, Sybils are always possible, and our electorate is an
admin-curated list. Out of scope for v1. Our attestations are a weak instance of accountability in the sense
of [PeerReview](https://doi.org/10.1145/1294261.1294279) (Haeberlen, Kouznetsov & Druschel, SOSP 2007): signed
statements, not complete logs with witness coverage, so we claim attribution, not detection completeness; and
unlike [Casper](https://arxiv.org/abs/1710.09437) (Buterin & Griffith, 2017) there is no stake to slash, leaving social
enforcement, which ERA independently concludes as well. Finally, **we are deliberately not fork-consistent**:
fork-linearizability ([Mazières & Shasha](https://doi.org/10.1145/571825.571840), PODC
2002; [SUNDR](https://www.usenix.org/conference/osdi-04/secure-untrusted-data-repository-sundr), OSDI
2004; [Cachin et al.](https://doi.org/10.1145/1281100.1281121), PODC 2007) makes divergence permanent so it is
detectable, whereas our supersede and catch-up rules exist to re-merge diverged members. That is the right trade for a
chat group, but this document does not borrow that vocabulary.
