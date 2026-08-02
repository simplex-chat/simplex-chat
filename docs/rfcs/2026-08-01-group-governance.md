# Democratic groups: replacing the admin set by member referendum

- [Motivation](#motivation), [Problem](#problem), [Solution](#solution)
- [Design](#design): [scope](#scope-v1), [member keys](#prerequisite-member-signing-keys-in-p2p-groups),
  [enabling](#enabling-governance-the-genesis-certificate), [electorate](#electorate),
  [events](#referendum-protocol), [transport](#transport-rules), [tally](#tally-majority-of-votes-cast),
  [duration](#duration-weak-support-waits), [certificates](#certificate-soundness),
  [applying](#applying-a-certificate), [catch-up](#catch-up-and-recovery),
  [tier 2](#tier-2-verification-gated-enfranchisement)
- [Threat model](#threat-model), [Limitations](#limitations), [Implementation sketch](#implementation-sketch)
- [Open questions](#open-questions), [Future work](#future-work), [Related work](#related-work)

## Motivation

From a member's point of view, a SimpleX group belongs to whoever created it, permanently. That person, or whoever
they promoted, or whoever ends up holding their device, can rename the group, rewrite its rules, remove any member,
silence them for everyone, and delete the whole thing. Members have no recourse except leaving: rebuilding elsewhere,
losing the history, re-inviting everyone by hand and hoping they follow. In a handful of friends this may be fine. In
a small or mid-sized community, one that has outgrown its founding circle and would carry on if any particular person
left, it means the community is a guest in someone else's space.

The failure modes are not hypothetical, and this project has already hit them: "we already had several accidental
deletions or lost owner accounts" (`2024-03-14-super-peers.md`). Today an owner who loses their device takes the
group's future with them; the spec notes that if the only owner leaves, the group can no longer be deleted, and nobody
can update its profile or fix its link. A compromised owner is worse, and an owner who simply becomes hostile is worst
of all, because the software is entirely on their side.

Nathan Schneider calls this default *implicit feudalism*, [*Governable Spaces: Democratic Design for Online
Life*](https://www.ucpress.edu/books/governable-spaces/paper) (University of California Press, 2024): "a bias, both
cultural and technical, for building communities as fiefdoms", in which platforms nudge users to tolerate nearly
all-powerful admins and benevolent dictators for life. His argument is that this is a design choice rather than a
technical necessity, and that it teaches its own politics: people whose everyday online spaces are never
self-governing stop expecting self-governance anywhere. A messenger that implements only the owner role can host only
fiefdoms, however decentralised its transport. This RFC does not make groups democratic by default; many groups should
stay exactly as they are. It makes democracy *available*: an opt-in, per-group mechanism so that a community which
wants to hold its admins accountable can do so inside the app, with voice rather than only exit.

## Problem

Admin power is absolute and unaccountable. Any admin can remove members and demote other admins, only owners can touch
owners, and there is no recovery when owners are inactive, lost, or hostile. Worse, admins *are* the group
infrastructure: they are the only message forwarders for not-yet-connected pairs (`isUserGrpFwdRelay`), the only
legitimate introducers of new members, and the threat model already grants them the ability to MITM introductions,
drop or modify forwarded messages selectively, and "disrupt decentralized group state by sending different messages...
to different group members" (`docs/protocol/simplex-chat.md`).

Democratic self-governance was named as an aspiration in `2024-03-14-super-peers.md`, and `2023-05-02-groups.md`
concluded that "some sort of consensus protocol is still needed for all membership changes other than member
addition". `2024-04-01-super-peers-2.md` drafted an approval vocabulary (`MemberApproval`, `GroupConsensus`), but with
quorums drawn from admins and owners only; the general membership holds no vote anywhere.

Three constraints are inherited from the stack. Deniability is a hard design goal below the chat layer, so
third-party-verifiable votes must be application-level signatures, the path already taken for channel roster events.
At the SMP layer only a queue's own parties hold its keys and rotation is a two-party negotiation, so admins have no
handle on other members' direct connections; a hosting router can drop a queue's messages wholesale, but each member
picks its own routers, so that risk is uncorrelated with admin power. And interactive BFT consensus among mobile
clients was assessed as impractical (`2023-10-20-group-integrity.md`): the design below needs none, only asynchronous
collection of signatures and a deterministic local tally.

## Solution

A group can opt in to **governed mode**. Enabling converts all owners to admins; a governed group has no owners, and
governed clients reject any event that would create or promote one. Governance parameters are fixed at enabling.

**No member becomes an admin except by certificate.** That covers four verbs, not three: no admin may promote a member
to admin, demote an admin, remove an admin, or **admit a new member at admin role or above**. The last is the one an
enumeration naturally misses, and it is not theoretical: `checkHostRole` is `memberRole < GRAdmin || memberRole <
memRole` (`Subscriber.hs:3578-3580`), both comparisons strict, so an equal-role admission passes today and an admin
can introduce accounts already carrying `GRAdmin` through `xGrpMemNew`, `xGrpMemIntro` or `xGrpMemFwd`. A governed
client therefore accepts a role above `GRModerator` only for a member in its **certified admin set**, and caps
every other admitted or assigned role at `GRModerator`. The certified set is the one sealed by genesis, then replaced
by each applied `GAReplaceAdmins`. Genesis must therefore carry the initial admin set explicitly, not only
`memberSetHash`: incumbent admins appear in no certificate, so without it a joiner has nothing to check a claimed
`GRAdmin` against, and the two available readings are both broken. Capping the whole roster it learns through
`xGrpMemIntro` leaves the joiner with no forwarder (`isMemberGrpFwdRelay`), no one who can moderate or remove, and no
one who can edit the profile, which makes the group inoperable for it; exempting join-time roster intros instead lets
an admin admit a Sybil at `GRAdmin` that every future joiner then accepts as a pre-existing admin. The admin set is a
handful of member IDs, so carrying it costs little. Tier 2 does not close this, because
enfranchisement gates voting and `E` rather than the role, so packed admins would keep forwarding, introduction and
removal powers in a tier-2 group. This is the core of the design, and it also closes a regression the design would
otherwise introduce. The existing receiver check permits acting on a member of *equal* role (`senderRole < memberRole`
is strict), so unilateral removal already reaches peers; what bounds it today is that only owners can touch owners,
and most groups have one or two. Abolishing owners without this rule would widen that power from the owner set to
every admin, leaving a rogue admin able to remove all its colleagues and then the membership.

Any member may propose a new admin set. Members vote aye or nay with Ed25519 signatures over the proposal hash, sent
over their direct connections. Anyone can assemble the votes into a **certificate**, a self-authenticating proof that
the proposal passed, which every member validates independently and applies atomically under a monotonic governance
version. The tally is a **simple majority of votes cast** (`A > B`), paired with a duration that falls as support
rises: a certificate may be evaluated only after `maxReferendumDays × max(0, 1 − 2A/E)`, counted from when that member
first saw the proposal. Weak support waits; a majority of the whole electorate is ripe at once.

Incumbents cannot block a referendum in progress. Governance events are self-authenticating and forwardable by any
member, a removed member keeps its vote for a referendum period, and certificates need no admin cooperation to be
assembled, transported or applied.

**This is tier 1 of a two-tier design, and its guarantee is attributability rather than prevention.** Membership in a
p2p group is whatever the admins have told each member, so an admin can put identities into the electorate that do not
exist and vote with them. Nothing in this tier stops that. What it does provide is that every such move is *visible to
every member*: fabricated members arrive as ordinary member-added events, purges are broadcast, every vote and
certificate is signed and attributable, and the result of every referendum is inspectable after the fact. Read that
claim strictly: it covers what an admin **sends**. An admin's other lever is to send nothing, and silence produces no
artifact to attribute. Withholding a member-added event, a proposal, or a certificate from a chosen receiver is
invisible at that receiver and indistinguishable from an offline peer, and it is the residue tier 1 does not address
(see [Limitations](#limitations)). Tier 1 is
therefore appropriate for a group that already trusts its admins not to attack it, and wants recourse and an audit
trail when one turns out to be careless, compromised, or simply wrong. It is not appropriate against an admin who is
hostile from the outset. [Tier 2](#tier-2-verification-gated-enfranchisement) closes that, at a usability cost, by
making enfranchisement depend on something admins cannot manufacture.

## Design

### Scope (v1)

- p2p groups only (`useRelays = false`). Relay groups and channels are future work: they are single-owner today, and
  replacing their owner set needs changes to the short-link owner chain in simplexmq.
- Referendum actions: replace the set of `GRAdmin` members, and remove members; tier 2 adds enfranchisement. Moderator
  roles are untouched. Removal is in v1 because otherwise a group can only purge accounts an admin planted by first
  electing new admins and asking them to act, which leaves a window, and because a group whose admins refuse to act has
  no other remedy. The action is a sum type, so profile changes, parameter changes and group deletion can be added
  without redesign.
- Groups of roughly 5 to 50 members, degrading past 100. Below five, referenda are ceremony over conversation.
- Tier 1 assumes admins are trusted not to fabricate members, and relies on members noticing if one does. Groups
  needing more should wait for tier 2.

### Prerequisite: member signing keys in p2p groups

Channels already give every member a per-group Ed25519 key (`GroupMember.memberPubKey`), and the wire format already
distributes keys in p2p groups: `MemberInfo` carries `memberKey`, populated by `memberInfo`, and travels in
`XGrpMemNew`, `XGrpMemIntro` and `XGrpMemFwd`. The verification path exists too, since `withVerifiedMsg` verifies
`CBGroup` signatures with prefix `smpEncode chatBinding <> smpEncode (memberId, pubKey)`.

Missing: generating and persisting a per-group keypair, populating `memberPubKey`, and pinning received keys as
`applyMemberKeyRole` does for channels. Existing members announce their key on upgrade, and governance cannot be
enabled until every member's key is known.

**The key must be pinned from the direct handshake, not from the introducer.** In p2p the introducing admin builds
`MemberInfo` from its own database, and the direct connection that follows carries no key at all: `XGrpMemInfo`
exchanges only a member id and profile. Pinning the introducer's copy would therefore let an admin hand every later
joiner's victim a key it holds itself, sign that joiner's votes, and see the joiner's real votes rejected as a
conflicting binding, invisibly and for every member added after enabling. `XGrpMemInfo` must carry the member's key,
that value is what gets pinned, and an introducer's copy is a hint to be confirmed rather than the trust anchor.

**A prerequisite bug fix, outside governance.** `xGrpMemIntro` accepts the introduced member's role verbatim: unlike
`xGrpMemNew` and `xGrpMemFwd`, its p2p branch inlines only the first conjunct of `checkHostRole`
(`Subscriber.hs:3223`, the introducer must itself be `GRAdmin`) and omits the `memberRole < memRole` cap, so an admin
can introduce a member carrying role
`GROwner` into a victim's local view, and that fake owner then satisfies every owner-gated check there. This is a
pre-existing hole with consequences beyond governance: `memInfo` reaches `createIntroReMember` verbatim
(`Subscriber.hs:3232`) and the role is written straight through (`Store/Groups.hs:2495`). `xGrpMemIntro` must cap the
introduced role at the introducer's; the relay branch just above it (`Subscriber.hs:3213-3221`) already does this via
`unknownMemberRole` and is the template.

### Enabling governance: the genesis certificate

Preconditions: every current member supports the governance chat version, and every member's key is known.

Enabling requires an **authorising set**: all current owners for a group that has them, or a strict majority of
current members for an ownerless group, each signing with their member key. The ownerless case exists because an empty
owner set would otherwise be satisfied by an empty signer set. The set must be non-empty.

The initiator broadcasts `x.grp.gov.enable` carrying a random 256-bit `governanceId`, the parameters, and signed bytes
  `smpEncode ("SXGG", groupIdentity, governanceId, params, memberSetHash, admins)`. Receivers validate, fail-closed
  on any failure, that:

1. the group is **not already governed**. A governed group is owner-free, so its owner set is empty and the
   owned-group rule would be vacuously satisfied by an unsigned certificate; without this check an attacker could
   re-genesis at will, resetting the parameters and the version chain. An identical repeat is a no-op; a different
   genesis for a governed group is surfaced, not applied;
2. the authorising set matches the rule above and is non-empty;
3. the parameters are within bounds: `7 ≤ maxReferendumDays ≤ 30`, `24 ≤ challengeHours ≤ 168`. Both ends bind. An
   unbounded window would make every non-unconditional referendum unresolvable; a one-day period with a one-hour
   window would let two confederates replace the admin set inside a day, irreversibly;
4. `admins` matches the receiver's own current admins **plus its current owners**, since genesis demotes owners to
   admins and `admins` is the post-demotion set, and `memberSetHash` matches its own current member list. A receiver
   with a join in flight may fail this
   through nobody's fault, so a failing genesis is retained, re-evaluated as membership settles, and may be re-served
   on request.

It then applies atomically: store governance state, demote all `GROwner` members to `GRAdmin`, set version 1.

**Joiners receive the governance state with the group.** A member that has none is not merely uninvolved: it still
records a `GROwner`, so `XGrpDel` is live against it, and check 1 above ("not already governed") is vacuous, so a
hostile host can hand it a private genesis with an attacker-chosen `governanceId` and thereafter it rejects every real
governance event as a mismatch, invisibly and permanently. The genesis certificate is therefore part of what a joiner
is given, any member re-serves it on request, and a client that sees a governance event for a group it believes
ungoverned requests the genesis rather than dropping the event. Governance state is also compared, along with
`governanceId`, whenever members exchange versions, so a divergent one surfaces.

From here no member holds `GROwner`, and governed clients reject any event that would create or set one, so `XGrpDel`
has no valid sender and the "delete the group to pre-empt a vote" attack is closed. Owner-gated checks for `XGrpInfo`
and `XGrpPrefs` relax to `GRAdmin` on both sides, since an owner-free group could otherwise never edit its profile
again. For new groups the creator enables at creation, a self-signed genesis over a membership of one.

### Electorate

> The **electorate** of a proposal is the set of members the receiver records as current at counting time, plus those
> it recorded as current when it first saw that proposal, plus those it had removed within the `maxReferendumDays`
> before it first saw that proposal, less any member removed by an applied certificate. Role is ignored and blocked
> members are included. A member may vote if and only if it is in that set, and `E` is its size.

Throughout: `A` is valid aye votes, `B` valid nays, `T = A + B` turnout, `E` the electorate size.

One set decides who may vote, the denominator of the thresholds, and who may propose. Eligibility deliberately ignores
role and `blockedByAdmin`, since both are levers incumbents control.

**The set is whatever the admins have said it is, and tier 1 does not pretend otherwise.** An admin can announce
members who do not exist, at any time including mid-referendum, and vote with them: `E + 1` fabricated identities give
an instant unconditional certificate, and fewer make any live proposal dead. Adding them mid-referendum pays twice,
since `2(A + k) > E + k` reduces to `2A + k > E`, so each fabricated aye also lowers the bar. Counting only votes from
members the receiver has itself connected to does not help: in p2p, connections are created automatically by the
introduction flow and admins control introductions, so a victim can be made to "connect to" a scripted identity within
minutes, and a rule that looks like a Sybil defence and is not is worse than none. Neither does an authenticated
membership log, because the log's own genesis must still be validated against the same admin-asserted list. Only
out-of-band verification closes it, which is [tier 2](#tier-2-verification-gated-enfranchisement); at tier 1
fabrication is merely *visible*.

Deflation is the same lever run backwards, and quieter. Because `E` is what the *receiver* recorded, an admin that
never relays some member-added events to a chosen receiver leaves it with a small `E`, so a certificate that is a
minority of the real group is unconditional and instantly ripe there. Inflation needs identities everyone can see
arrive; deflation needs only silence, and the victim's view is indistinguishable from a small group. Divergent `E` is
visible in the applied-certificate announcements of step 8, whose tally reveals a receiver that treated a small aye
count as unconditional, which is detection rather than prevention.
[Tier 2](#tier-2-verification-gated-enfranchisement) repairs it.

Additions to the electorate are read at counting time and never frozen at first sight. Freezing them would let an
admin manufacture a small `E` without withholding anything, merely by delivering the proposal ahead of the outstanding
member-adds, and would make a member that joins legitimately mid-referendum an elector at some receivers and not
others, so the same certificate bytes would yield different tallies.

**An action may name only members that the receiver recorded when it first saw the proposal, or has recorded
since**, whether they are current or removed-but-still-stored, **except that a `GAEnfranchise` subject is exempt
from this check and from the
tier-2 enfranchised-set check.** The exemption is not a loophole, it is the only way the rule has a fixpoint: an
enfranchisement subject is by construction outside the enfranchised set, and at a receiver whose view of it was
deflated it is outside the recorded set too, so requiring membership in either would mean nobody beyond the founding
electorate could ever be enfranchised. What establishes the subject instead is the agreeing-`subjectKey` majority of
claims itself, which carries both its identity and its key.

Evaluating candidacy at apply time alone would hand any incumbent a one-message veto over every election: remove one
named candidate before the certificate ripens and the action names a non-member, so the certificate is void,
repeatable indefinitely and selectively. Including `firstSeenAt`, which the receiver already stores, closes that.
Including later knowledge as well is what keeps the rule from replacing that veto with an ordinary race: a member
added shortly before a proposal is recorded by some receivers before they see it and by others after, and freezing
candidacy at first sight alone would make the same certificate permanently void at the latter, for the same reason the
electorate is not frozen at first sight. The union of the two is monotone, so a rejection for naming an unrecorded
member is provisional and re-evaluated as membership settles.

Naming a member that is no longer present is then tolerated rather than void, since the action changes roles rather
than presence, and presence is repaired where an incumbent removed it: **applying `GAReplaceAdmins` first reinstates
as current members any named members the receiver removed after that proposal's `firstSeenAt`, other than by an
applied certificate.** Their rows still exist and their connections are still live, because a governed group defers
teardown (transport rule 5). Without reinstatement the anchor buys nothing: an incumbent that cannot void the action
by removing one candidate removes every candidate instead, each named member is then absent, setting an absent member
to `GRAdmin` is a no-op while demoting every incumbent is not, and the abort below fires, one message per candidate
and repeatable against every proposal. Reinstatement is bounded to members this receiver itself removed during this
referendum, so it cannot introduce anyone the receiver has not already recorded, and once applied the reinstated
member is an admin, so no remaining admin may remove it again. **What remains is a backstop: apply aborts if the
resulting admin set would still be empty after reinstatement**, which covers the one case the group cannot be
protected from, a candidate that left of its own accord. An
ownerless, adminless p2p group cannot forward for unconnected pairs (`isUserGrpFwdRelay`, `Subscriber.hs:3694`), admit
anyone (`Subscriber.hs:3223`), or remove anyone (`Subscriber.hs:3674-3675`), and the referendum needed to recover has
no forwarder.

**Removed members still vote, if they were members when the referendum reached the receiver.** Because `E` does not
appear in the pass condition `A > B`, a removal that only shrank the denominator would *lower* the bar a purger has to
clear. A removal that lands after a receiver first saw a proposal has no effect on that proposal at all: the member
was current at `firstSeenAt`, so it stays in that referendum's electorate and its vote keeps counting until the
referendum resolves. Keeping the removed in the vote as well as the count is what makes a purge futile for one
referendum period and gives the group that period to answer with a referendum the purged can vote in. Recency is
measured by the receiver's own record of the removal, never by anything an author writes.

Two conditions bound it. The allowance applies only to members the receiver already recorded when it first saw the
proposal, so identities added *during* a referendum get none: otherwise an admin facing an ouster could add accounts
while the vote ran, lose it, and still have those accounts protected from the incoming admins for a further month,
which is long enough to carry a second referendum reinstating them. And it does not apply to removals enacted by
certificate, which take effect at once, because the allowance exists to blunt unilateral action and a certificate is
the opposite of that.

**`E` never decreases within a referendum.** A receiver keeps the largest electorate it has computed for a given
proposal. A denominator that can be made too small passes fraudulent certificates, which is fatal; one made too large
only delays honest ones, which is recoverable.


### Referendum protocol

Five events, all version-gated JSON:

```haskell
XGrpGovEnable                        -- genesis, above
  { governanceId :: ByteString, params :: GovParams, memberSetHash :: ByteString
  , admins :: [MemberId]              -- the initial certified admin set
  , signers :: NonEmpty (MemberId, Signature) }

XGrpGovPropose
  { governanceId, govVersion :: Int64
  , action :: GovAction               -- GAReplaceAdmins | GARemoveMembers
                                      -- | GAEnfranchise (tier 2, exactly one member)
                                      -- sorted, non-empty, every named member recorded by the
                                      -- receiver at firstSeenAt or since (candidacy rule below)
  , prevProposalHash :: ByteString    -- proposal *held* at the previous version,
                                      -- or the genesis hash at govVersion = 2
  , proposer :: MemberId, sig :: Signature }
  -- proposalHash = sha256 (smpEncode ("SXGP", groupIdentity, governanceId, govVersion, action,
  --                                   prevProposalHash, proposer))

XGrpGovVote
  { governanceId, proposalHash, voter :: MemberId, vote :: Aye | Nay, sig }
  -- sig over smpEncode ("SXGV", groupIdentity, governanceId, proposalHash, voter, vote).
  -- `voter` is inside the preimage: omitting it lets one signature verify as a
  -- vote from any member.

XGrpGovCert
  { governanceId, proposalHash, votes :: [(MemberId, Vote, Signature)] }
  -- announcement form omits `votes`, carrying {proposalHash, certHash, tally}

XGrpGovRequest
  { governanceId, haveVersion :: Int64, proposalHash :: Maybe ByteString
  , requester :: MemberId, sig :: Signature }
```

The request carries a signature although it authorises nothing, because rule 1 below says governance events are
self-authenticating and any member may forward them, and that premise has to hold for all five. Unsigned, it can be
forged in a victim V's name with `haveVersion = 0`, and every member that honours it sends V a full catch-up bundle:
`gov_served_version` bounds each responder individually but not the aggregate, so V receives N-1 bundles and exhausts
its queue quota (`defaultMsgQueueQuota = 128`, `simplexmq Server/Env/STM.hs:255-256`), surfacing as `SMP.QUOTA` and
marking the connection inactive. Serving is additionally bound to the connection the request arrived on.

`govVersion` must be the receiver's stored version + 1 for full processing; a higher claim is not retained and only
marks a possible gap, triggering one rate-limited request with backoff. Multiple proposals may coexist at a version; a
client retains the first from each proposer and ignores that proposer's later ones, so nobody can refresh a deferral
timer by re-proposing. `prevProposalHash` is validated on every path, not only during catch-up: it must name a
proposal the receiver **holds** at the previous version, or the genesis hash. Holds, not applied: version skipping is
permitted and same-version supersede means members legitimately apply different proposals at one version, so requiring
the chain to run through what a member applied would fail every later proposal for anyone that skipped a version or
took the superseded branch. Left unvalidated it is a free 32-byte nonce,
and since it sits inside `proposalHash` an attacker could grind the mandate-order tie-break at will.

Every signature binds a `groupIdentity`, so a member key cannot be replayed across groups; `governanceId` is chosen by
the enabler and binds nothing on its own. **p2p groups have no such identifier today**: `publicGroupId` lives in
`GroupKeys`, which is `Nothing` outside channels, and the p2p verification branch binds only `(memberId, pubKey)`.
Establishing a per-group identity is therefore a second prerequisite alongside member keys, not something this design
can assume. The *genesis
hash* is the hash of the signed genesis bytes, `certHash` the hash of the canonical certificate encoding, and
*canonical certificate bytes* means the votes sorted by `MemberId` and encoded deterministically, which is what makes
mandate order identical at every member.

Conflicting signed votes from one member on one proposal annul that member's vote, which keeps vote-set union
order-independent. Both signatures are retained and re-served, since a client that holds only one cannot show anyone
else why it annulled the vote. Signatures are detached and over deterministic encodings so third parties can re-aggregate them
into certificates; this differs from shipped message signing, which signs transmitted bytes in an envelope. At ~100
bytes per vote, groups beyond ~120 members need the chunked-blob transport already used for roster blobs.

### Transport rules

1. All `x.grp.gov.*` events are added to `isForwardedGroupMsg` and exempt from `expectedForwarder` and the admin-only
   forwarder check: they are self-authenticating, so any member may forward or rebroadcast them. An admin dropping
   them achieves nothing while any other path exists. **Governance events must not use the existing `sharedMsgId`
   dedup**; they are deduplicated on a hash of their own signed content. The existing duplicate check keys on
   `(group_id, shared_msg_id)` alone: `duplicateGroupMsgMemberIds` filters on those two columns and selects
   `author_group_member_id` only to report it (`Store/Messages.hs:318-328`), raising `SEDuplicateGroupMessage` at
   `Store/Messages.hs:313-314`. Signature verification does run first, since `withVerifiedMsg` wraps `processEvent`
   (`Subscriber.hs:1046-1047`) and `createNewRcvMessage` is reached only from `saveGroupRcvMsg` inside it
   (`Subscriber.hs:1062`), and it is no defence here, because the poisoning message is the attacker's own and carries
   the attacker's own valid signature. Any member that has seen a proposal can therefore replay its `sharedMsgId` on a
   well-formed message of its own to a member that has not, and the genuine proposal is then dropped as a duplicate at
   that member on every path, permanently. Content-hash dedup
   also makes rebroadcast idempotent, which the multi-path rule needs to avoid a rebroadcast storm.
2. Governance events are exempt from `blockedByAdmin` forwarding suppression, and from `XGrpMemRestrict` restrictions
   generally, including the per-member send limits of `2025-02-17-member-send-limits.md`: those are set by a member of
   equal or higher role, so leaving them in force would give an admin a throttle on a proposer's governance traffic.
3. Governance events must not be placed behind the `memberCanSend` role gate (`Subscriber.hs:1705-1713`), which drops
   *received* messages from any member at `GRObserver`. The gate covers only `XMsgNew`, `XMsgUpdate` and
   `XGrpDirectInv` today, so this costs nothing to honour, but it is fatal to rule 4 if overlooked: an admin demoting a
   proposer to observer would not merely mark it, it would make every recipient silently discard that member's
   proposals and votes.
4. Demotion and blocking never affect voting, since eligibility depends only on membership, and given the exemptions
   above they do not affect delivery either.
5. **Removal deferral.** In a governed group, `XGrpMemDel` for a member does not tear down that member's connections
   for `2 × maxReferendumDays`, and `x.grp.gov.*` continues to flow over them in both directions. Deferral is armed by
   the
   group being governed, *not* by holding a proposal, since arming on an unresolved proposal is defeated by ordering:
   purge first and the victims lose their connections before any proposal exists, so they can neither receive it nor
   vote, while still counting in `E`. Deferral lasts `2 × maxReferendumDays`, because a member removed at `t` is within
   the recency allowance of any referendum first seen before `t + maxReferendumDays`, which can resolve as late as
   `t + 2 × maxReferendumDays`. It does not apply to a member removed *by certificate*, consistent with those removals
   taking effect at once.

### Tally: majority of votes cast

The proposal passes iff `A > B`. Ties fail, so the status quo wins a split vote, and abstention is neutral.

There is no turnout quorum, because participation thresholds reward abstention: an opponent defeats a quorumed
proposal more cheaply by boycotting it than by voting against it. The rule is also chosen for a property the protocol
cannot otherwise supply, that a member can predict the effect of its own vote without knowing the turnout. Under a
turnout-weighted curve the ayes required move as votes arrive, and a nay can raise the bar for the ayes.

Taken alone the rule has an obvious weakness: with no nays, a single aye passes. The answer is time, not a higher
threshold.

### Duration: weak support waits

A certificate becomes **ripe**, meaning it may be evaluated at all, only after

```
ripeAt = firstSeenAt + maxReferendumDays × max(0, 1 − 2A/E)
```

`firstSeenAt` is when *this* member first saw the proposal, and it is the only clock in the design. Proposals carry no
timestamp, so there is nothing to backdate and a proposal concealed for a year does not arrive pre-aged: its
recipients start their clocks on receipt. `latestClose = firstSeenAt + maxReferendumDays` is the local worst case.

Support buys speed and nothing else. Half the electorate is ripe at once, a quarter waits half the maximum, and a
single aye waits almost the full 30 days, throughout which one nay defeats it. At `2A ≥ E` the delay reaches zero, so
"a majority of the whole electorate passes immediately" is where this curve meets the axis rather than a separate
rule. Symmetrically a proposal is **dead** once `2B ≥ E`.

Only ayes move the clock. A member who objects must never hasten the outcome it objects to, and an opposition bloc
must never accelerate a vote by turning up. Ripeness is local, deliberately: what must be identical everywhere is the
tally and the mandate ordering, and neither reads `firstSeenAt` or `E`.

### Certificate soundness

Because the tally counts nays against the proposal, a hostile assembler would omit them, so validation must be robust
to selective inclusion.

- A certificate is **unconditional** if it would pass with every member not in it counted as nay. Substituting
  `B ← B + (E − T)` into `A > B` gives exactly `2A > E`, so no vote the receiver has not yet seen can flip it and it
  applies at once, unless its action is `GARemoveMembers` or `GAEnfranchise`, which always observe the window for the
  reason below. The test asks whether any outstanding vote could change the outcome, so it is independent of the
  tally rule. The word to be careful with is *outstanding*: it quantifies over votes not yet cast or not yet arrived,
  and does not cover a vote already counted being withdrawn, which is what annulment does when a conflicting signature
  arrives from a member already counted as an aye. At `2A = E + 1` one annulment flips the result. For reversible
  actions that is tolerable, since the group can answer with another referendum. For removal it is not, and removal is
  the action for which the unconditional requirement exists, so **a removal certificate must be unconditional *and*
  observe the challenge window** rather than applying at once: the window is the only interval in which annulment
  evidence is collected, and skipping it for precisely the irreversible action inverts the intent.
- Any other valid certificate opens a local **challenge window** of `challengeHours`, starting at
  `max(local first processing, ripeAt)`. First processing means when this client decoded and validated it, not when it
  was delivered: anchoring on delivery would give a member offline for a month a window that closed before it looked.
  The window closes `challengeHours` after the most recent previously-unseen valid nay, capped at one
  `maxReferendumDays` beyond the initial close, so cohorts learning of a result at different times can aggregate while
  a drip-feed cannot stall resolution. At close the member evaluates over the union of certificate votes and its own.

A member that first sees a proposal at or after its own ripeness may still vote until its window closes. Once a member
applies or finally rejects a certificate, further votes for that proposal are ignored, except inside a catch-up bundle.

### Applying a certificate

On receiving `x.grp.gov.cert`, a member:

1. fetches the referenced proposal if it lacks it, and validates it: signature, `prevProposalHash`, and an action
   naming only members the receiver recorded at `firstSeenAt` or has recorded since, current or removed-but-stored,
   with the `GAEnfranchise` subject exempt; a proposal rejected only for naming a member not yet recorded is retained
   and re-evaluated as membership settles, as a failing genesis is;
2. verifies every vote signature and discards annulled votes;
3. checks `govVersion` **greater than** the stored version. A stale version is ignored as replay, except that a
   same-version certificate for a *different* proposal supersedes the applied one if it ranks higher in **mandate
   order**: larger aye count first, then smaller `proposalHash`. Ranking on ayes is deliberate. Margin (`A − B`) is
   the better measure of a mandate but is grindable, since `B` is whatever the assembler chose to include, whereas
   omitting *ayes* only weakens the omitter. Both components come from canonical certificate bytes, so the order is
   total and identical at every member;
4. for a version gap greater than one, requires a **witness chain**: one bundle per intervening version whose
   certificate passes the tally on its own vote set, each linked by `prevProposalHash`. This is a cheap check and is
   not load-bearing; what binds is step 6;
5. waits until `ripeAt`, then runs the challenge window and re-evaluates over the union, unless the certificate is
   unconditional **and** its action is `GAReplaceAdmins`. `GARemoveMembers` and `GAEnfranchise` are required to be
   unconditional by step 7, so exempting unconditional certificates in general would mean the two irreversible actions
   never open a window at all; the window is where annulment evidence arrives, and at `2A = E + 1` one annulment flips
   the tally;
6. if the certificate is stale, meaning for a version the receiver has already passed, or arrived by catch-up,
   requires **attestations**: `min(3, N)` distinct members, outside the certificate's aye set, must have signed
   `smpEncode ("SXGS", groupIdentity, governanceId, govVersion, proposalHash)` for *that proposal*, where `N` is the
   number of members of the receiver's electorate that are outside the aye set **and are not the receiver itself**,
   and a member issues one only for a certificate it has itself applied. When `N = 0` no attestation is required and
   the certificate must instead be unconditional (`2A > E`): the aye set is then every member the receiver records but
   itself, and demanding corroboration from a set whose only member is the receiver would strand exactly the lagging
   member this step exists to recover. Comparing versions alone would let honest
   attestations for the legitimate certificate at a version authorise a fabricated rival at the same version. The
   threshold is a small constant rather than a fraction of `E`: scaling it would let an admin brick every catch-up in
   the group by announcing phantoms, since `E` is admin-asserted, and the `min` covers a near-unanimous
   certificate that leaves fewer than three *other* members outside its aye set. Three attestations are a cost
   multiplier against a small conspiracy, not a bound against a
   determined one;
7. applies atomically. For `GAReplaceAdmins`, first reinstate as current members any named members the receiver
   removed after that proposal's `firstSeenAt` other than by an applied certificate, then set every named member to
   `GRAdmin` and demote every other current `GRAdmin` to `GRMember`. For `GARemoveMembers`, remove every named
   member, effective on the electorate at once and
   with no recency allowance; **a removal certificate must be unconditional (`2A > E`) *and* must observe the challenge
   window**, since removal is the one action with no inverse (there is no `GAAddMembers`, and a removed member can no
   longer vote or be named). Without the first, a single unanswered aye in a quiet week would empty the group; without
   the second, an annulment that would have flipped a knife-edge tally arrives after the members are already gone. For
   `GAEnfranchise`, which names exactly one member, enfranchise it; that certificate must likewise be unconditional
   and observe the challenge window, since enfranchisement has no inverse either and an enfranchised Sybil is a
   witness for the next one.
   Abort the apply if the resulting admin set would still be empty after reinstatement. Then store the version,
   proposal and certificate;
8. announces the applied certificate once to all connections as `{proposalHash, certHash, tally}`. The announcement is
   a display-only hint that peers verify by fetching the certificate. It exists because a same-version disagreement
   produces no version gap and so no catch-up trigger, and without it two halves of a group would never compare.

`GAReplaceAdmins` names a complete set rather than a delta, so applying it is idempotent and order-independent.
`GARemoveMembers` is neither: it is a delta with no inverse, so a receiver that applied a removal before a
higher-ranked same-version `GAReplaceAdmins` superseded it keeps the members removed, while a receiver that saw them in
the other order never removes them, and no action exists to reconcile. **An applied removal is therefore final and is
not undone by supersede**, and clients state it that way rather than claiming order-independence they do not have.
Clients
should surface a "contested result" state while two same-version certificates are in play, and new admins should avoid
destructive actions until their certificate's window has closed unchallenged.

### Catch-up and recovery

A member can fall behind: it rejected a knife-edge certificate others applied, or it was offline. On seeing traffic
referencing a higher version it sends `x.grp.gov.request`; any member re-serves, per applied version above
`haveVersion`, the proposal and its as-applied vote set, bounded per requester by the version last served to them and
rate-limited. Responses also carry the hashes of active proposals at the requester's new version + 1, so a member
advancing by catch-up regains its vote in the live referendum.

Version skipping is allowed: each certificate is a complete, independently endorsed set, so a member that could not
validate version N can adopt N+1 directly.

Recovery repairs lag and missing information. It cannot force a member to accept a tally its own held votes
contradict; such a member stays behind until a later certificate it can validate arrives. A member whose only
reachable peers are the presenter and the certificate's aye set cannot satisfy the attestation bound, unless the aye
set covers every member it records but itself, and otherwise stays pending, which is the correct fail-closed outcome
and is surfaced rather than masked.

### Tier 2: verification-gated enfranchisement

Tier 1 leaves one hole, and everything else in this document is downstream of it: the electorate is asserted by the
same parties governance is meant to constrain. Tier 2 closes it with the one primitive in SimpleX that an admin cannot
manufacture, because it requires a human at the other end.

SimpleX already implements out-of-band verification: two members compare a code over a channel the protocol does not
carry, through `APIVerifyGroupMember`. An admin can fabricate a member, introduce it, and hold its key; it cannot make
you compare codes with a person who does not exist, and it cannot substitute itself into a comparison you make in a
video call or in a room. That is the property tier 2 needs, but the existing mechanism cannot be used as-is, for three
reasons that each require a change.

**Verify the member's governance key, not the connection.** In p2p groups `APIVerifyGroupMember` verifies the pairwise
*connection*: it stores `connections.security_code`, not `memberVerifiedCode`, which is the channel-only field. Three
consequences follow. It needs an established connection, and connections are created by admin-brokered introductions,
so an admin could deny enfranchisement forever by never introducing a member, silently and deniably. It is per-session,
and it is **cleared automatically on ratchet re-sync**, so an admin that is a member's forwarder can induce a desync
and silently revoke verification, which presents as network trouble and leaves no artifact. And it says nothing about
the member's signing key, which is what votes are checked against. Tier 2 therefore verifies a fingerprint of the
member's governance key, which needs no connection, survives re-sync, and binds exactly the object in question.

**Verification must be published, as a signed claim.** The enfranchisement test is global (a majority of the electorate
must have verified X) but verification is written only in the verifier's own database and never transmitted. No
receiver can evaluate the rule from local state. A verifier therefore publishes `smpEncode ("SXGKV", groupIdentity,
governanceId, proposalHash, subject, subjectKey)` signed with its own key, which is an aye vote on `GAEnfranchise`
and is what makes the referendum evaluable. Such a claim is forgeable by anyone holding the verifier's key, so tier 2
is no stronger than the keys of its existing electorate: it converts fabrication-at-will into key-compromise, which is
the point, but it is not a step to a different trust root.

`proposalHash` is in the preimage for the same reason `voter` is in a vote's. Without it a claim is a standing aye on
every present and future `GAEnfranchise` naming that subject, so a member removed by referendum could be re-enfranchised
by replaying the original claims with no human re-verifying anything, and a verifier could never withdraw. With it, the
human act and the signed artifact separate cleanly: comparing the fingerprint is done once and recorded locally, and
the client re-signs per proposal from that local record, which a verifier can decline.

The rule is one line: **in a tier-2 group, a member is enfranchised only once a strict majority of the current
electorate has verified it out of band.** Everything else in this document is unchanged: the tally, the clock,
certificates, mandate order and the admin-set rule all operate on the enfranchised set rather than on the recorded
members, and `E` is the count of enfranchised members.

A majority rather than a fixed count, because a constant would be self-defeating: an attacker that once controlled
`k` enfranchised identities could enfranchise unboundedly many more, each new one becoming a witness for the next,
which is the seeding problem that defeats every fixed threshold. Requiring a majority means growing the electorate
requires already controlling it, and an attacker that controls a majority has won by ordinary means and needs no
fabrication. It also removes a parameter from the enabler's hands.

Mechanically the rest is not new machinery. It is an ordinary referendum on a new action, `GAEnfranchise MemberId`,
whose aye votes are exactly the signed verification claims above, and which reuses the certificate, ripeness, mandate
order and audit trail unchanged. Two clauses are specific to it and both are load-bearing.

**An enfranchisement certificate must be unconditional** (`2A > E`), stated explicitly and not inherited. The ordinary
pass condition is `A > B`, and under it a single aye with no nays enfranchises: one compromised identity signs a claim
for a Sybil, nobody thinks to vote nay against a stranger, the certificate ripens, and the Sybil is then itself a
witness for the next. That is precisely the seeding the majority rule exists to prevent, reproduced with `k = 1`.
`2A > E` is the strict majority the rule states, and it must appear as its own clause exactly as it does for removals,
together with the same challenge-window requirement.

**`GAEnfranchise` names exactly one member.** A claim binds one `subject`, so a list would need an aggregation rule,
and the natural one (a verifier's claim for any named subject counts as its aye on the action) is catastrophic: twenty
people who verified one newcomer would enfranchise `[P, S1 ... S9]` unconditionally, nine Sybils riding on twenty
humans' verification of P. Singleton actions make the question not arise. Enfranchising several members is several
referenda, which is also the honest presentation of the cost.

**Claims must agree on `subjectKey`.** Only claims naming the same key count toward the same tally. Without this,
verifiers who pinned different keys for X all produce valid claims, X is enfranchised, and X's votes then verify at
some members and not others, permanently.

**Enfranchisement is evaluated once and does not lapse.** A member enfranchised when the electorate was ten stays
enfranchised when it is forty, even though its original supporters are no longer a majority. Recomputing against the
live electorate would make the predicate self-referential, since the electorate is what it defines. The certificate is
applied once, like any other, and the result is stored.

What this buys is precise. Fabricated identities cannot vote, cannot be counted in `E`, cannot propose, and cannot be
named by an action, so the whole family of attacks in which an admin manufactures an electorate disappears rather than
being mitigated. It also repairs key binding as a side effect: verification confirms the key of the member you
verified, so a MITM-ing introducer is caught by the same act, and the pinning problem tier 1 cannot solve stops
mattering. This is where the tier-1 prerequisite that a key be pinned from the direct handshake rather than from the
introducer is superseded rather than relied on: an agreeing majority of claims is itself a stronger pinning path than
either, and a tier-2 client takes it as authoritative.

**Tier 2 therefore repairs a deflated electorate, which tier 1 cannot.** At tier 1 a receiver never told about X has
no way to learn X's key, because keys arrive only in admin-originated member-adds, and the unknown-member record
created when another admin forwards X's traffic (`createNewUnknownGroupMember`, `Groups.hs:3483-3502`) carries no key.
At tier 2 the claims themselves carry `subjectKey`, are signed by members the receiver already knows, and travel as
ordinary governance events forwardable by anyone, so both X's key and X's enfranchisement can be reconstructed over a
path incumbents do not control.

What it costs is equally precise, and it is not small.

Verification is manual, and a majority is a lot of it: admitting one member to the electorate of a thirty-member group
means sixteen people each comparing a fingerprint with the newcomer out of band. In practice the electorate closes as
the group grows, and a tier-2 group should expect to enfranchise in occasional deliberate rounds rather than
continuously. Whether that is a defect or the point is a judgement about the group: it is the same threshold the group
uses for every other decision, and an electorate that admits members more cheaply than it makes decisions is the
weaker link. Keys are also per group, so the same human must be verified again in every tier-2 group shared with them,
which follows from the unlinkability the key design exists to preserve.

The primitive does not exist yet. The only key-derived code today is `channelMemberCode` (`Types.hs:1912-1920`), which
is pairwise and channel-only; p2p uses the double-ratchet AD hash, unrelated to any signing key. Tier 2 needs a
single-member fingerprint over the governance key, comparison UI presenting it as a property of the person rather than
of the session, and a p2p-writable store column.

A member that joins after enabling cannot reconstruct the founding enfranchised set. Genesis carries `memberSetHash`,
not the member set, and members present at enabling are enfranchised without verification, so a joiner takes that set
from an admin and its `E` is admin-asserted, which is the thing tier 2 exists to remove. Later enfranchisements are
self-authenticating through their claims, so the exposure shrinks as the group turns over without reaching zero.
Carrying the member set itself in the genesis event would close it and is the obvious v2 change.

The trusted setup is load-bearing, for the same reason. Members present at enabling are enfranchised unverified, so a
genesis sealed over identities an owner fabricated inherits them permanently and no later verification undoes it. The
security of the whole tier reduces to the honesty of the group at the moment it opts in.

It can produce cliques, where a well-connected subgroup is enfranchised and a peripheral one is not, which is a
fairness problem and not merely a usability one. An admin can also shrink the electorate by removing enfranchised
members, lowering the majority that the remainder needs to admit new ones; that is visible and bounded by the recency
allowance, but it is a real path and it is not closed here. And none of this touches classic Sybil: an admin who
recruits `n` real people, or verifies with `n` humans it controls, enrols `n` genuine voters. That residual is
irreducible without a central authority, and is the same one every democracy has.

Tiers are per group and chosen at enabling, and v1 offers no way to change tier afterwards. Switching is not merely
unimplemented: moving to tier 2 shrinks the electorate, which makes `2A > E` *easier* and ripeness *shorter*, so it
would hand whichever clique had verified each other an instant unconditional certificate. A tier change is a parameter
change and belongs with `GAChangeGovernance` in future work.

## Threat model

This complements the [group threat model](../protocol/simplex-chat.md#threat-model), which continues to apply in full.
Only entries specific to governed groups are listed.

#### A group owner, before governance is enabled

*can:* decide unilaterally whether the group is ever governed, and with what parameters within the bounds; shape the
founding electorate by removing members first; seal a genesis reflecting what it has told a particular member.

*cannot:* set parameters outside the bounds; retain any privilege after enabling; re-enable, reset or reparameterise
governance later; delete a governed group.

#### A group admin, in a governed group

*can:* everything the existing group threat model grants an admin, including MITM of introductions and selective
forwarding; delay governance traffic to members that depend on it for forwarding; remove ordinary members and
moderators at any time; announce members who do not exist, or withhold real ones, since membership is admin-asserted.

*can also:* slow any referendum, and deny unconditionality outright, by announcing members nobody has met, since
those count in `E` even though their votes do not, and since inflating `E` lengthens the very delay the admin needs;
silence a member permanently by purging it and repeating the purge each referendum period; and, facing an ouster,
manufacture a future majority by adding accounts and connecting them while the vote runs. That last path is bounded
rather than closed: accounts added during a referendum get no recency protection, so the incoming admins can purge
them the moment they win, and a referendum may itself name members for removal. An admin who seeds accounts *before*
anyone proposes is not bounded by anything here.

*can also, at tier 1:* fabricate members and vote with them, up to and including carrying any referendum outright,
since membership is admin-asserted; hold the key of any member whose introduction it MITMed, and annul that member's
vote by signing a conflicting one. Both are visible to members but not prevented, and both are what tier 2 closes.

*cannot:* change the admin set outside a referendum, so it cannot remove its colleagues, pack the set by promotion or
by admitting new members at admin role, or entrench
itself; veto a referendum by procedure, since no admin signature appears in propose, vote or apply, candidacy is
judged as of each receiver's first sight of the proposal or later, and a candidate removed while the referendum runs
is reinstated by the certificate that names it; censor one, since governance events are forwardable by any
member; silence a voter by demotion or blocking, by removal within the recency period, or by removal at any point
after that receiver first saw the proposal, since a member current at `firstSeenAt` stays in that referendum's
electorate however it is removed later; rush a result, since speed
must be bought with votes and no timestamp a proposer writes is read by any rule; empty the group with a thin
referendum, since removal certificates must be unconditional. It *can* reach the same end in two steps, by passing a
thin `GAReplaceAdmins` that installs cooperating admins and having those admins remove members with ordinary admin
powers, which governance does not touch. The unconditional rule bounds what a certificate does, not what the admins a
certificate installs may then do.

#### A group member, in a governed group

*can:* propose at any time, and vote on every proposal at the current version; forward and serve any governance event,
including to members an admin will not relay to; assemble a certificate, and omit nays from it; attest its applied
state, and so be recorded as having vouched for a result; see how everyone voted, since there is no ballot secrecy;
equivocate its own vote, at the cost of annulling it.

*cannot:* vote twice on a proposal without annulling; vote after departing; pass a proposal naming an empty admin set
or non-members; pass a proposal quickly without support in a group of meaningful size, though in a group of two or
three the delay is hours rather than weeks.

#### A colluding minority

*can:* withhold a proposal and present it together with a certificate, capturing members whose windows do not
aggregate enough nays; bank a passing certificate and enact it later through the corroborated path; capture a member
whose only reachable peers are the conspiracy; with a genuine majority, do anything the group can do.

*cannot:* satisfy the attestation bound with attestations for a different proposal, and cannot satisfy it with its own
supporters *below four members*, which is where the bound bites and where it stops: one aye plus three confederates
outside the aye set clears `min(3, ·)`, and the bound is waived outright if it holds an aye from every member of the
receiver's electorate but the receiver, exactly as step 6 concedes when it calls the constant a cost multiplier against
a small conspiracy rather than a bound against a determined one. Note
that "a genuine majority of the receiver's electorate" is only meaningful at tier 2: at tier 1 the electorate is
authored by admins, so a colluding admin reaches any majority it likes.

## Limitations

- **At tier 1 the electorate is admin-asserted, and that is the design's defining limitation.** An admin can put
  identities into the electorate that do not exist, hold their keys and vote with them, and can withhold real members
  from others' rosters. An admin of a group of *n* can announce *n* scripted identities and carry any referendum
  outright, including one removing everyone else. What tier 1 offers instead is that each step is observable: the
  accounts arrive as ordinary member-added events, and the votes and certificate are signed and attributable. That is
  a real property for a group whose admins are trusted and occasionally wrong, and none at all against one hostile from
  the start. Tier 2 is the answer; classic Sybil, where an attacker enrols real confederates, is not solvable here at
  either tier (Douceur) and is left to future work. An admin facing removal can also spend the referendum period
  building a majority for the *next* one; accounts added mid-referendum get no recency protection and can be purged
  the moment it resolves, but nothing touches accounts seeded before any proposal exists.
- **Member keys are only as good as the introduction that carried them.** Tier 1 pins a member's key from the direct
  handshake rather than the introducer's assertion, but the introducer relays the invitation for that handshake and can
  substitute it, which the group threat model already grants as an admin capability. An admin that MITMs an
  introduction holds the key both sides pin, and can sign a conflicting vote in that member's name to have the genuine
  one annulled. Out-of-band verification is the only fix, which is to say tier 2.
- **Enabling is the moment of maximum exposure, and joiners cannot check it at all.** Genesis is validated against
  local knowledge, so an owner that has equivocated membership beforehand can seal a skewed electorate. Worse, a
  member who joins *after* enabling can never satisfy check 4, because `memberSetHash` is frozen at the enabling-time
  membership and the joiner's own list necessarily differs; joiners therefore accept the governance state they are
  given, on the same footing as everything else they learn at join. They should compare `governanceId` with a member
  they did not learn about from their host, which detects a planted genesis but only after the fact. Enable
  governance in a group whose membership is visibly settled.
- **A purge still works, slowly.** Recency and deferral together keep a purged member voting and reachable for one
  referendum period, and keep it voting for the whole of any referendum already under way when the purge lands; after
  that the removals stand. That buys the group `maxReferendumDays` to answer with a
  referendum the purged can vote in, not immunity, and an incumbent who repeats the purge each period wins by
  attrition against a group that stops paying attention.
- **Equivocated votes are a partition tool, not just an accident.** A confederate votes aye, lets a certificate apply
  at half the group, then sends its conflicting nay only to the other half, whose tally now fails. Catch-up cannot
  repair it, since it cannot force a member to accept a tally its own held votes contradict, so repeating this keeps a
  chosen subset permanently out of step at the cost of one identity and no admin powers. Retaining both signatures
  makes the equivocation provable, which is attribution rather than prevention.
- **Deferral hands a removed member a channel the status quo does not.** Removals do not sever connections for
  `2 × maxReferendumDays`, and governance events over them are exempt from every moderation lever, so a harasser removed
  today keeps a live queue to a targeted member for weeks and can fill it against the 128-message quota, where today
  that member is disconnected in seconds. This is a capability the feature creates, and the clearest respect in which
  enabling governance is worse than not.
- **Omission is the unhandled class.** The design is built around making positive acts signed, ordered and
  attributable, and it does that. It has no answer to an admin that acts by not acting. Because admins are the sole
  forwarders for pairs that have not yet connected, withholding is a per-receiver, per-message capability, and every
  use of it presents as ordinary network trouble: a proposal that never arrives, a member whose existence a receiver is
  never told about (deflating that receiver's `E`), a member never introduced and so never verifiable at tier 2, an
  admit timed for when a target is offline. The mitigations are partial and all detective rather than preventive:
  governance events are forwardable by any member, so a proposal reaches anyone with one honest path; and applied
  attestations expose divergent views once members compare them. A group that cannot compare notes out of band gets
  no protection from either.
- **Two lagging members can deadlock each other.** The attestation waiver covers a single member behind a
  near-unanimous certificate. Two or more members that are all behind, whose non-aye set consists only of each other,
  still cannot attest for one another and stay pending until one receives the certificate as ordinary live traffic
  rather than in a catch-up bundle.
- **A voluntary departure can still void an election.** Reinstatement repairs candidates an incumbent removed, but
  not a candidate that left of its own accord, so a certificate naming only such candidates aborts on the empty-admin
  backstop and the incumbents keep their roles.
- **Governance traffic is unsuppressible.** The exemptions that make censorship hard mean no moderation lever closes
  that channel; a per-peer budget is needed at implementation time. The factor that should set the constant is
  certificate verification: proposals are bounded to the first from each proposer and votes by a uniqueness
  constraint, but nothing bounds certificates per proposal per sender, and each costs up to `E` signature
  verifications, so one message buys `E` units of work from every peer.
- **Small electorates get hours, not weeks.** At `E = 2` a single aye is half the electorate, so the delay is zero and
  only the challenge window protects; at `E = 3` it is 10 days. The month-long deterrent exists from about `E = 10`.
- **Knife-edge divergence.** Non-unconditional certificates can resolve differently across members whose windows saw
  different vote sets. Version lag is repairable by catch-up; genuine tally disagreement persists until a later
  certificate the stranded member can validate. This is still better than the status quo, where any admin state change
  can diverge arbitrarily and undetectably.
- **Acceptance is not a pure function of the certificate.** Ranking is canonical, but acceptance unions locally held
  votes. This is the one deviation from strong eventual consistency and the root of the case above.
- **No ballot secrecy.** Votes are signed and verifiable within the group, so incumbents can identify nay voters and
  retaliate. It is the price of certificates any member can carry past a hostile forwarder.
- **Governance is a one-way door.** There is no path to disable it or change its parameters in v1, and a governed
  group can never be deleted remotely. It also cannot be deleted *locally* by a current member unless the owner
  assertions are relaxed: `APIDeleteChat` requires `isOwner || not (memberCurrent membership)`
  (`Commands.hs:1338-1341`), so in an owner-free group every current member's delete throws. Leaving first and then
  deleting works, but that is a workaround, not a design.
- **The lost-device case is unsolved.** An owner who lost their device has not left, so the `GROwner` record persists
  and only an owner can remove an owner. Closing it needs a third authorising rule keyed on demonstrable
  unreachability.

## Implementation sketch

- `Protocol.hs`: `GovAction` (`GAReplaceAdmins`, `GARemoveMembers`, and `GAEnfranchise` for tier 2), the five event
  types and tags (added to
  `isForwardedGroupMsg`), deterministic binary encodings with domain separation (`SXGG`/`SXGP`/`SXGV`/`SXGS`).
- **Prerequisite:** the `xGrpMemIntro` role cap, for all p2p groups rather than only governed ones.
- Key management: per-group member keypair for p2p governed groups, population of `memberPubKey` on join and
  introduction, TOFU pinning as in `applyMemberKeyRole`.
- Sign and verify `XGrpMemNew`/`XGrpMemDel`/`XGrpMemRole` in governed p2p groups through the existing p2p branch in
  `withVerifiedMsg`, which independently closes the unsigned-forward forgery hole.
- `Subscriber.hs`: handlers for the five events, added both to `isForwardedGroupMsg` and to the separate receive-side
  accept list in `processForwardedMsg`, which is a manually synced `case` and is easy to miss; genesis validation
  (checks 1 to 4); proposal validation including `prevProposalHash` on every path; ripeness from a persisted
  per-proposal `first_seen_at`, with `E` as a per-proposal running maximum; the challenge-window worker with late
  voting; the apply procedure above; catch-up serving with per-requester bounds; forwarder and `blockedByAdmin`
  exemptions; removal deferral; certificate removals applied without the recency allowance; **capping any admitted or
  assigned role at `GRModerator` except for members in the certified admin set**; **rejection of any admin-set change
  outside a certificate**, and of owner-role
  members in `xGrpMemNew`/`xGrpMemIntro`/`xGrpMemFwd`/`xGrpMemRole`.
- `Commands.hs`: relax the `GROwner` assertions for governed groups, not only the one in `runUpdateGroupProfile`.
  The sweep is `Commands.hs:1192` (root key), `1338-1341` (`APIDeleteChat`), `1911`, `2718` (`APIAddGroupRelays`),
  `4020` (`runUpdateGroupProfile`), `4088`, and `checkSendAsGroup` (`Subscriber.hs:1065-1067`). Channel-only sites are
  included, since the rule is that a governed client rejects any event that would create or set an owner.
- Store:

```sql
ALTER TABLE groups ADD COLUMN governance TEXT;            -- params + governanceId; null = not governed
ALTER TABLE groups ADD COLUMN governance_version INTEGER;
ALTER TABLE group_members ADD COLUMN gov_served_version INTEGER;
ALTER TABLE group_members ADD COLUMN gov_removed_at TEXT;  -- receiver's own record of when it removed the member;
                                                           -- electorate, recency and reinstatement read this,
                                                           -- never anything an author writes
CREATE TABLE group_referenda (
  referendum_id      INTEGER PRIMARY KEY,
  group_id           INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  proposal_hash      BLOB    NOT NULL,
  gov_version        INTEGER NOT NULL,
  action             BLOB    NOT NULL,
  prev_proposal_hash BLOB    NOT NULL,
  first_seen_at      TEXT    NOT NULL,  -- local ripeness anchor; not from the wire
  electorate_max     INTEGER NOT NULL,  -- running maximum of |E|; membership is derived, not stored
  proposer_member_id BLOB    NOT NULL,
  proposal_sig       BLOB    NOT NULL,
  status             TEXT    NOT NULL,  -- active / passed / failed / superseded
  applied_cert       BLOB,              -- as-applied vote set, re-served on catch-up
  attestations       BLOB,              -- attestations received, for re-serving
  window_closes_at   TEXT               -- null until a certificate opens the challenge window
);
CREATE TABLE group_referendum_votes (
  referendum_id INTEGER NOT NULL REFERENCES group_referenda ON DELETE CASCADE,
  voter_id      BLOB    NOT NULL,  -- MemberId: a vote may arrive from a member with no local row
  vote          TEXT    NOT NULL,
  vote_sig      BLOB    NOT NULL,
  annulled      INTEGER NOT NULL DEFAULT 0,
  UNIQUE (referendum_id, voter_id, vote)  -- both sides of an equivocation are kept as evidence
);
```

- API: `APIEnableGroupGovernance`, `APIProposeGroupAdmins`, `APIGroupVote`. Certificate assembly, application and
  catch-up are automatic. Chat items for proposal, votes, result and contested result.

## Open questions

1. Should v1 ship an action to change or disable governance, rather than making enabling a one-way door?
2. Should the tally carry a minimum-support floor (`A ≥ max(3, ⌈E/3⌉)`)? The clock makes an unsupported proposal wait
   a month, during which one nay defeats it, but a proposal nobody looks at still carries. A floor closes that, at the
   cost of a second clause and of deadlocking a group whose active population falls below it.
3. Should acceptance be made a pure function of the certificate, as ranking is, trading the anti-vote-withholding
   defence for convergence?
4. Deferral and catch-up budgets are two-sided DoS tuning and need concrete constants: too tight and purges become
   schedulable, too loose and reference spam suspends moderation.
5. Should the genesis certificate require unanimity of owners, as here, or a majority?

## Future work

- **Authenticated membership.** The largest gap. Any signed membership structure must solve what the cut draft did
  not: a genesis that does not reduce to admin assertion, and enfranchisement that does not depend on admin-brokered
  introductions.
- **Relay groups and channels.** The same certificate machinery can gate the channel roster, but replacing *owners*
  needs threshold updates to the short-link owner chain and link-queue authority in simplexmq, where owners are ranked
  and any single owner key controls the link. That is the step-2 RFC, and where this meets the roadmap item "Multisig:
  M-of-N approval for administrative actions".
- **More actions:** change parameters, update profile and preferences, delete the group, replace moderators.
- **Tier 3: stronger Sybil defence.** Tier 2 stops fabricated members but not an admin who recruits real ones.
  Reputation or contribution weighting (`2024-03-14-super-peers.md` suggests a "community score"), social-graph
  analysis, or proof-of-personhood would each raise that bar, and each brings its own fairness cost. Also **ballot
  secrecy** for groups that need it, which is incompatible with self-authenticating certificates as specified.

## Related work

**Whether this is possible at all.** Under Herlihy's consensus hierarchy ([Wait-Free
Synchronization](https://cs.brown.edu/~mph/Herlihy91/p124-herlihy.pdf), TOPLAS 13(1), 1991), Frey, Gestin and Raynal
compute the synchronization power of access-control objects ([The Synchronization Power of AllowList and
DenyList](https://arxiv.org/abs/2302.06344), DISC 2023): an AllowList has consensus number 1, a *k*-DenyList has
consensus number *k*, and the entire difference is the **anti-flickering** property, once denied never allowed again.
Promoting admins is AllowList-shaped and free; demoting them with revocation semantics would be DenyList-shaped, and
since every member verifies admin authority, *k* is the whole group, which is unattainable asynchronously by
[FLP](https://doi.org/10.1145/3149.214121) (Fischer, Lynch & Paterson, JACM 32(2), 1985). We therefore decline
anti-flickering deliberately: a demoted admin can be re-recognized, by a later referendum and transiently during a
contested window. Revisable finality is the price of implementability rather than a shortcoming, which is why this
document never claims revocation. The same shape holds for payments in [The Consensus Number of a
Cryptocurrency](https://arxiv.org/abs/1906.05574) (Guerraoui et al., PODC 2019), and reconfiguration is likewise not
inherently consensus-hard (Aguilera, Keidar, Malkhi & Shraer, DynaStore, JACM 58(2), 2011), though that result assumes
crash faults and read/write storage rather than Byzantine faults and policy.

**What consistency model this is.** [Byzantine Eventual Consistency](https://arxiv.org/abs/2012.00472) (Kleppmann &
Howard, 2020) characterises the boundary by I-confluence and suggests exactly our split: aggregate I-confluently, then
decide the winner. Vote accumulation is I-confluent, since vote sets union and the annulment rule is defined to keep
that order-independent; the winner is decided by mandate order rather than by consensus. One honest gap: strong
eventual consistency (Shapiro, Preguiça, Baquero & Zawirski, INRIA RR-7506, 2011) requires state to be a function of
the updates received, and while ranking is pinned to canonical bytes, *acceptance* unions locally held votes. That is
the deviation recorded under limitations, and closing it would trade the anti-vote-withholding defence for
convergence.

**Duelling admins.** [ERA](https://arxiv.org/abs/2601.22963) (Dougal, PaPoC '26), from Element, is the closest
published work: two admins concurrently revoking each other, where revocation is non-monotonic and forces rollbacks.
Its critique of Kleppmann's *seniority ranking* proposal, that a junior can never revoke a senior and that a revoked
admin can backdate to fake concurrency, applies directly to SimpleX's existing `roleRequiredToChange`, and is an
argument for this RFC. We remove the duel rather than arbitrate it: authority comes from a majority certificate rather
than from another admin, and the whole set is replaced atomically, so there is no revocation cycle. We reject its
*finality arbiter*, a mutually trusted ordering peer, because that is the chokepoint this design exists to remove, and
its fallback of a "Creator" arbiter is unacceptable when the creator may be who the group needs to remove.
Consequently we do not get its bounded total order.

**Practice in group messaging.** [MLS](https://www.rfc-editor.org/rfc/rfc9420.html) (RFC 9420) advances linear epochs
and lets the Delivery Service serialize concurrent commits, which is unavailable to us by construction and is why we
need mandate order where MLS needs only a server. [DCGKA](https://doi.org/10.1145/3460120.3484542) (Weidner,
Kleppmann, Hugenroth & Beresford, CCS 2021) is serverless and explicitly tolerates the same membership flickering, "a
user may be removed and re-added, possibly indirectly", and scopes authorization policy out, which is the gap this
document fills. [More is Less](https://eprint.iacr.org/2017/713) (Rösler, Mainka & Schwenk, EuroS&P 2018) found group
management messages unauthenticated in deployed messengers, which is the empirical case for signing membership events
here.

**The voting rule.** May's theorem characterises simple majority as the unique rule that is anonymous, neutral and
positively responsive, so departures need a justification specific to the setting. We reject a turnout quorum for its
abstention incentive, and because on-chain governance shows turnout far too low for fixed quorums to be met in any
case ([Feichtinger et al.](https://arxiv.org/abs/2302.12125); Fritsch, Müller & Wattenhofer, "Who controls DAOs?").
We also reject [Polkadot's](https://arxiv.org/abs/2005.13456) adaptive quorum biasing, despite its appeal: its
required support as a fraction of the electorate is `p²/(1−p)` in the opposition share `p`, hence independent of group
size, so it lets a small bloc win whenever opposition is thin. Its safety rests on a standing class of attentive
opponents, which token governance has and a private group does not, and the DAO turnout evidence often cited for it
describes the wrong population. The cost of dropping it is real: at low turnout AQB was the stricter rule, and we
recover that on a different axis by making weak support wait. We reject ballot secrecy ([Juels, Catalano &
Jakobsson](https://doi.org/10.1145/1102199.1102213), WPES 2005) because verifiable signatures are what let a
certificate travel past a hostile forwarder, at the cost recorded under limitations.

**Sybil resistance and accountability.** [The Sybil Attack](https://doi.org/10.1007/3-540-45748-8_24) (Douceur, IPTPS
2002) states the limit this design inherits: without a central authority, Sybils are always possible, and our
electorate is an admin-curated list. Social-graph defences (Yu et al., SybilGuard and SybilLimit) need a fast-mixing
trust graph and are found by Alvisi et al. (IEEE S&P 2013) to be essentially community detection, which fits a
co-membership graph of this size poorly. Our attestations are a weak instance of accountability in the sense of
[PeerReview](https://doi.org/10.1145/1294261.1294279) (Haeberlen, Kouznetsov & Druschel, SOSP 2007), signed statements
rather than complete logs with witness coverage, so we claim attribution rather than detection completeness; and
unlike [Casper](https://arxiv.org/abs/1710.09437) (Buterin & Griffith, 2017) there is no stake to slash, leaving
social enforcement. Finally, we are deliberately **not** fork-consistent: fork-linearizability ([Mazières &
Shasha](https://doi.org/10.1145/571825.571840), PODC 2002;
[SUNDR](https://www.usenix.org/conference/osdi-04/secure-untrusted-data-repository-sundr), OSDI 2004; [Cachin et
al.](https://doi.org/10.1145/1281100.1281121), PODC 2007) makes divergence permanent so it is detectable, whereas
catch-up and same-version supersede exist to re-merge diverged members. That is the right trade for a chat group, but
this document does not borrow that vocabulary.
