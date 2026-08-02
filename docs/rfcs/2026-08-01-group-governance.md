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

**The admin set then changes only by referendum.** No admin may promote a member to admin, demote an admin, or remove
one. This is the core of the design, and it also closes a regression the design would otherwise introduce. The
existing receiver check permits acting on a member of *equal* role (`senderRole < memberRole` is strict), so unilateral
removal already reaches peers; what bounds it today is that only owners can touch owners, and most groups have one or
two. Abolishing owners without this rule would widen that power from the owner set to every admin, leaving a rogue
admin able to remove all its colleagues and then the membership.

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
certificate is signed and attributable, and the result of every referendum is inspectable after the fact. Tier 1 is
therefore appropriate for a group that already trusts its admins not to attack it, and wants recourse and an audit
trail when one turns out to be careless, compromised, or simply wrong. It is not appropriate against an admin who is
hostile from the outset. [Tier 2](#tier-2-verification-gated-enfranchisement) closes that, at a usability cost, by
making enfranchisement depend on something admins cannot manufacture.

## Design

### Scope (v1)

- p2p groups only (`useRelays = false`). Relay groups and channels are future work: they are single-owner today, and
  replacing their owner set needs changes to the short-link owner chain in simplexmq.
- Two referendum actions: replace the set of `GRAdmin` members, and remove members. Moderator roles are untouched.
  Removal is in v1 because otherwise a group can only purge accounts an admin planted by first electing new admins and
  asking them to act, which leaves a window, and because a group whose admins refuse to act has no other remedy. The
  action is a sum type, so profile changes, parameter changes and group deletion can be added without redesign.
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
`xGrpMemNew` and `xGrpMemFwd`, its p2p branch has no `checkHostRole`, so an admin can introduce a member carrying role
`GROwner` into a victim's local view, and that fake owner then satisfies every owner-gated check there. This is a
pre-existing hole with consequences beyond governance; `xGrpMemIntro` must cap the introduced role at the introducer's.

### Enabling governance: the genesis certificate

Preconditions: every current member supports the governance chat version, and every member's key is known.

Enabling requires an **authorising set**: all current owners for a group that has them, or a strict majority of
current members for an ownerless group, each signing with their member key. The ownerless case exists because an empty
owner set would otherwise be satisfied by an empty signer set. The set must be non-empty, and every signer must be a
member the receiver has itself connected to.

The initiator broadcasts `x.grp.gov.enable` carrying a random 256-bit `governanceId`, the parameters, and signed bytes
  `smpEncode ("SXGG", groupIdentity, governanceId, params, memberSetHash)`. Receivers validate, fail-closed on any
  failure, that:

1. the group is **not already governed**. A governed group is owner-free, so its owner set is empty and the
   owned-group rule would be vacuously satisfied by an unsigned certificate; without this check an attacker could
   re-genesis at will, resetting the parameters and the version chain. An identical repeat is a no-op; a different
   genesis for a governed group is surfaced, not applied;
2. the authorising set matches the rule above, is non-empty, and every signer is connected to the receiver;
3. the parameters are within bounds: `7 ≤ maxReferendumDays ≤ 30`, `24 ≤ challengeHours ≤ 168`. Both ends bind. An
   unbounded window would make every non-unconditional referendum unresolvable; a one-day period with a one-hour
   window would let two confederates replace the admin set inside a day, irreversibly;
4. `memberSetHash` matches the receiver's own current member list. A receiver with a join in flight may fail this
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

> The **electorate** is the set of current members the receiver records, at any role and including blocked members,
> plus those it has removed within the last `maxReferendumDays`. A member may vote if and only if it is in that set,
> and `E` is its size.

Throughout: `A` is valid aye votes, `B` valid nays, `T = A + B` turnout, `E` the electorate size.

One set decides who may vote, the denominator of the thresholds, and who may propose. Eligibility deliberately ignores
role and `blockedByAdmin`, since both are levers incumbents control.

**The set is whatever the admins have said it is, and tier 1 does not pretend otherwise.** An admin can announce
members who do not exist, at any time including mid-referendum, and vote with them: `E + 1` fabricated identities give
an instant unconditional certificate, and fewer make any live proposal dead. An earlier draft tried to blunt this by
counting votes only from members the receiver had itself connected to. That rule is dropped, because it does not work:
in p2p every member-to-member connection is created automatically by the introduction flow with no user involvement,
and admins control introductions, so an admin can have every victim "connect to" a scripted identity within minutes. A
rule that looks like a Sybil defence and is not is worse than none, because it invites reliance. The honest statement
is that fabrication is prevented at tier 2 and merely *visible* at tier 1.

**An action may name only members that the receiver recorded when it first saw the proposal.** Evaluating candidacy at
apply time instead would hand any incumbent a one-message veto over every election: remove one named candidate before
the certificate ripens and the action names a non-member, so the certificate is void, repeatable indefinitely and
selectively. Anchoring on `firstSeenAt`, which the receiver already stores, closes that; naming members who have since
left is harmless, since the action changes roles rather than presence.

**Recently removed members still vote, if they predate the referendum.** Because `E` does not appear in the pass
condition `A > B`, a removal that only shrank the denominator would *lower* the bar a purger has to clear. Keeping the
removed in the vote as well as the count is what makes a purge futile for one referendum period and gives the group
that period to answer with a referendum the purged can vote in. Recency is measured by the receiver's own record of
the removal, never by anything an author writes.

Two conditions bound it. The allowance applies only to members the receiver already recorded when it first saw the
proposal, so identities added *during* a referendum get none: otherwise an admin facing an ouster could add accounts
while the vote ran, lose it, and still have those accounts protected from the incoming admins for a further month,
which is long enough to carry a second referendum reinstating them. And it does not apply to removals enacted by
certificate, which take effect at once, because the allowance exists to blunt unilateral action and a certificate is
the opposite of that.

**`E` never decreases within a referendum.** A receiver keeps the largest electorate it has computed for a given
proposal. A denominator that can be made too small passes fraudulent certificates, which is fatal; one made too large
only delays honest ones, which is recoverable.

**The electorate is admin-curated, and this design does not fix that.** Membership in a p2p group is whatever each
member's admins have told them, so an admin can announce members who do not exist, or withhold real ones, and the
electorate inherits both. An earlier draft proposed an authenticated membership log to close this. It is cut: its own
genesis still had to be validated against the same admin-asserted list, and enfranchisement within it depended on
admin-brokered introductions, so it gated the electorate on the very lever it was meant to remove while roughly
doubling the size of the design. See limitations and future work.

### Referendum protocol

Five events, all version-gated JSON:

```haskell
XGrpGovEnable                        -- genesis, above
  { governanceId :: ByteString, params :: GovParams, memberSetHash :: ByteString
  , signers :: NonEmpty (MemberId, Signature) }

XGrpGovPropose
  { governanceId, govVersion :: Int64
  , action :: GovAction               -- GAReplaceAdmins [MemberId] | GARemoveMembers [MemberId]
                                      -- sorted, non-empty, every named member a current
                                      -- member (not merely in E)
  , prevProposalHash :: ByteString    -- proposal applied at the previous version,
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
  -- announcement form omits `votes`, carrying {certHash, tally} instead

XGrpGovRequest
  { governanceId, haveVersion :: Int64, proposalHash :: Maybe ByteString }
```

`govVersion` must be the receiver's stored version + 1 for full processing; a higher claim is not retained and only
marks a possible gap, triggering one rate-limited request with backoff. Multiple proposals may coexist at a version; a
client retains the first from each proposer and ignores that proposer's later ones, so nobody can refresh a deferral
timer by re-proposing. `prevProposalHash` is validated on every path, not only during catch-up: it must name a
proposal the receiver holds at the previous version, or the genesis hash. Left unvalidated it is a free 32-byte nonce,
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
   forwarder check: they are self-authenticating, so any member may forward or rebroadcast them, with the existing
   `sharedMsgId` dedup. An admin dropping them achieves nothing while any other path exists.
2. Governance events are exempt from `blockedByAdmin` forwarding suppression, and from `XGrpMemRestrict` restrictions
   generally, including the per-member send limits of `2025-02-17-member-send-limits.md`: those are set by a member of
   equal or higher role, so leaving them in force would give an admin a throttle on a proposer's governance traffic.
3. Demotion and blocking never affect voting, since eligibility depends only on membership.
4. **Removal deferral.** In a governed group, `XGrpMemDel` for a member does not tear down that member's connections
   for `maxReferendumDays`, and `x.grp.gov.*` continues to flow over them in both directions. Deferral is armed by the
   group being governed, *not* by holding a proposal. An earlier rule armed it on holding an unresolved proposal, which
   an incumbent defeats by ordering: purge first, and the victims lose their connections before any proposal exists, so
   they can neither receive it nor return a vote, while still counting in `E` and thereby raising the bar for the
   referendum against the purger. Deferral must last exactly as long as the recency allowance it exists to make usable.

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
  `B ← B + (E − T)` into `A > B` gives exactly `2A > E`. No outstanding vote can flip it, so it applies at once. The
  test asks whether any outstanding vote could change the outcome, so it is independent of the tally rule.
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
   naming only electorate members;
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
5. waits until `ripeAt`, then, unless the certificate is unconditional, runs the challenge window and re-evaluates
   over the union;
6. if the certificate is stale, meaning for a version the receiver has already passed, or arrived by catch-up,
   requires **attestations**: `min(3, |non-aye members|)` distinct members, outside the certificate's aye set, must
   have signed `smpEncode ("SXGS", groupIdentity, governanceId, govVersion, proposalHash)` for *that proposal*, and a
   member issues one only for a certificate it has itself applied. Comparing versions alone would let honest
   attestations for the legitimate certificate at a version authorise a fabricated rival at the same version. The
   threshold is a small constant rather than a fraction of `E`: scaling it would let an admin brick every catch-up in
   the group by announcing phantoms, since `E` is admin-asserted, and the `min` is needed because a near-unanimous
   certificate may leave fewer than three members outside its aye set, which would otherwise strand every lagging
   member permanently. Three attestations are a cost multiplier against a small conspiracy, not a bound against a
   determined one;
7. applies atomically. For `GAReplaceAdmins`, set every named member to `GRAdmin` and demote every other current
   `GRAdmin` to `GRMember`. For `GARemoveMembers`, remove every named member, effective on the electorate at once and
   with no recency allowance; **a removal certificate must be unconditional** (`2A > E`), since removal is the one
   action with no inverse (there is no `GAAddMembers`, and a removed member can no longer vote or be named), and
   without that condition a single unanswered aye in a quiet week would empty the group. Then store the version,
   proposal and certificate;
8. announces the applied certificate once to all connections as `{proposalHash, certHash, tally}`. The announcement is
   a display-only hint that peers verify by fetching the certificate. It exists because a same-version disagreement
   produces no version gap and so no catch-up trigger, and without it two halves of a group would never compare.

Because the action names a complete set rather than a delta, application is idempotent and order-independent. Clients
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
reachable peers are the presenter and the certificate's aye set cannot satisfy the attestation bound and stays
pending, which is the correct fail-closed outcome and is surfaced rather than masked.

### Tier 2: verification-gated enfranchisement

Tier 1 leaves one hole, and everything else in this document is downstream of it: the electorate is asserted by the
same parties governance is meant to constrain. Tier 2 closes it with the one primitive in SimpleX that an admin cannot
manufacture, because it requires a human at the other end.

SimpleX already implements out-of-band member verification: two members compare a security code over a channel the
protocol does not carry, and the result is stored as `memberVerifiedCode` on the member record, set through
`APIVerifyGroupMember`. An admin can fabricate a member, introduce it, and hold its key; it cannot make you compare
codes with a person who does not exist, and it cannot substitute itself into a comparison you make in a video call or
in a room.

The rule is one line: **in a tier-2 group, a member is enfranchised only once `verifyCount` already-enfranchised
members have verified it out of band**, `verifyCount` being a genesis parameter with a small default. Everything else
in this document is unchanged: the tally, the clock, certificates, mandate order and the admin-set rule all operate on
the enfranchised set rather than on the recorded members. `E` is the count of enfranchised members. Verification is
already per-member local state, so the check needs no new wire format and no new trust root.

What this buys is precise. Fabricated identities cannot vote, cannot be counted in `E`, cannot propose, and cannot be
named by an action, so the whole family of attacks in which an admin manufactures an electorate disappears rather than
being mitigated. It also repairs key binding as a side effect: verification confirms the key of the member you
verified, so a MITM-ing introducer is caught by the same act, and the pinning problem tier 1 cannot solve stops
mattering.

What it costs is equally precise, and it is not small. Verification is manual and most people never do it, so a group
must run a deliberate round of code-checking before its first referendum, and the electorate is whoever bothered. That
is a different proposition from tier 1: governance by the members who have met each other, not by everyone in the
group. It bootstraps awkwardly, since the first members have nobody enfranchised to verify them and must be taken from
the genesis set. It can produce cliques, where a well-connected subgroup is enfranchised and a peripheral one is not,
which is a real fairness problem and not merely a usability one. And it does not touch classic Sybil: an admin who
recruits `n` real people, or who verifies with `n` humans it controls, enrols `n` genuine voters. That residual is
irreducible without a central authority, and is the same one every democracy has.

Tiers are per group and chosen at enabling. A group may start at tier 1 and move to tier 2 later, since raising the
bar only shrinks the electorate and needs no new genesis; the reverse is not offered.

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

*cannot:* change the admin set outside a referendum, so it cannot remove its colleagues, pack the set, or entrench
itself; veto a referendum by procedure, since no admin signature appears in propose, vote or apply and candidacy is
judged as of each receiver's first sight of the proposal; censor one, since governance events are forwardable by any
member; silence a voter by demotion or blocking, or by removal within the recency period; rush a result, since speed
must be bought with votes and no timestamp a proposer writes is read by any rule; empty the group with a thin
referendum, since removal certificates must be unconditional.

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

*cannot:* satisfy the attestation bound with its own supporters, or with attestations for a different proposal. Note
that "a genuine majority of the receiver's electorate" is only meaningful at tier 2: at tier 1 the electorate is
authored by admins, so a colluding admin reaches any majority it likes.

## Limitations

- **At tier 1 the electorate is admin-asserted, and that is the design's defining limitation.** An admin can put
  identities into the electorate that do not exist, hold their keys and vote with them; it can also withhold real
  members from others' rosters. Concretely, an admin of a group of *n* can announce and introduce *n* scripted
  identities, have every member's client auto-connect to them, and carry any referendum outright, including one that
  removes everyone else. No rule in tier 1 prevents this. What tier 1 offers instead is that each step is observable
  by every member: the fabricated accounts arrive as ordinary member-added events, the introductions are visible, the
  votes and certificate are signed and attributable, and the outcome can be inspected afterwards. That is a real
  property for a group whose admins are trusted and occasionally wrong, and no property at all against an admin that
  is hostile from the start. Tier 2 is the answer; classic Sybil, where an attacker enrols real confederates, is not
  solvable here at either tier (Douceur) and is left to future work.
- **Member keys are only as good as the introduction that carried them.** Tier 1 pins a member's key from the direct
  handshake rather than the introducer's assertion, but the introducer relays the invitation for that handshake and
  can substitute it, which the group threat model already grants as an admin capability. An admin that MITMs an
  introduction holds the key both sides pin, and can then sign a conflicting vote in that member's name and have the
  member's genuine vote annulled. First-seen pinning also has an ordering hazard, since the introducer's asserted copy
  arrives before the handshake. Out-of-band verification is the only fix, which is to say tier 2.
- **Enabling is the moment of maximum exposure, and joiners cannot check it at all.** Genesis is validated against
  local knowledge, so an owner that has equivocated membership beforehand can seal a skewed electorate. Worse, a
  member who joins *after* enabling can never satisfy check 4, because `memberSetHash` is frozen at the enabling-time
  membership and the joiner's own list necessarily differs; joiners therefore accept the governance state they are
  given, on the same footing as everything else they learn at join. They should compare `governanceId` with a member
  they did not learn about from their host, which detects a planted genesis but only after the fact. Enable
  governance in a group whose membership is visibly settled.
- **A purge still works, slowly.** Recency and deferral together keep a purged member voting and reachable for one
  referendum period; after that the removals stand. That buys the group `maxReferendumDays` to answer with a
  referendum the purged can vote in, not immunity, and an incumbent who repeats the purge each period wins by
  attrition against a group that stops paying attention.
- **An admin can answer a referendum by manufacturing an electorate.** Adding accounts is an admin power, votes count
  from any member the receiver has met, and an admin controls introductions, so an admin facing removal can spend the
  referendum period building a majority for the next one. Two rules bound it: accounts added after a member first saw
  the proposal get no recency protection, so they can be purged the instant the referendum resolves, and a referendum
  can itself remove members. Neither helps against accounts seeded before any proposal exists, which is the
  admin-curated electorate limitation above and the strongest argument for solving membership authentication.
- **Equivocated votes are a partition tool, not just an accident.** A confederate votes aye, lets a certificate apply
  at half the group, then sends its conflicting nay only to the other half, whose tally now fails. Catch-up cannot
  repair it, because it "cannot force a member to accept a tally its own held votes contradict". Repeating this on
  each referendum keeps a chosen subset permanently out of step at the cost of one identity and no admin powers.
  Retaining both signatures makes the equivocation provable and re-servable, which is attribution rather than
  prevention.
- **Deferral hands a removed member a channel the status quo does not.** Removals do not sever connections for
  `maxReferendumDays`, and governance events over them are exempt from every moderation lever, so a harasser removed
  today keeps a live queue to a targeted member for weeks and can fill it against the 128-message quota. Today that
  member is disconnected in seconds. This is a capability the feature creates, not a tuning parameter, and it is the
  clearest respect in which enabling governance is worse than not.
- **Governance traffic is unsuppressible.** The exemptions that make censorship hard mean no moderation lever closes
  that channel; a per-peer budget is needed at implementation time.
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
  group can never be deleted remotely.
- **The lost-device case is unsolved.** An owner who lost their device has not left, so the `GROwner` record persists
  and only an owner can remove an owner. Closing it needs a third authorising rule keyed on demonstrable
  unreachability.

## Implementation sketch

- `Protocol.hs`: `GovAction` (`GAReplaceAdmins`, `GARemoveMembers`), the five event types and tags (added to
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
  exemptions; removal deferral; certificate removals applied without the recency allowance; **rejection of any admin-set
  change outside a certificate**, and of owner-role members in `xGrpMemNew`/`xGrpMemIntro`/`xGrpMemFwd`/`xGrpMemRole`.
- `Commands.hs`: relax the `GROwner` assertion in `runUpdateGroupProfile` to `GRAdmin` for governed groups.
- Store:

```sql
ALTER TABLE groups ADD COLUMN governance TEXT;            -- params + governanceId; null = not governed
ALTER TABLE groups ADD COLUMN governance_version INTEGER;
ALTER TABLE group_members ADD COLUMN gov_served_version INTEGER;
CREATE TABLE group_referenda (
  referendum_id      INTEGER PRIMARY KEY,
  group_id           INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  proposal_hash      BLOB    NOT NULL,
  gov_version        INTEGER NOT NULL,
  action             BLOB    NOT NULL,
  prev_proposal_hash BLOB    NOT NULL,
  first_seen_at      TEXT    NOT NULL,  -- local ripeness anchor; not from the wire
  electorate_max     INTEGER NOT NULL,  -- running maximum of E
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
