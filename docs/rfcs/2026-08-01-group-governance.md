# Democratic groups: replacing the admin set by member referendum

- [Motivation](#motivation), [Problem](#problem), [Solution](#solution)
- [Design](#design): [scope](#scope-v1), [member keys](#prerequisite-member-signing-keys-in-p2p-groups),
  [enabling](#enabling-governance-the-genesis-certificate), [electorate](#electorate),
  [events](#referendum-protocol), [transport](#transport-rules), [tally](#tally-majority-of-votes-cast),
  [duration](#duration-weak-support-waits), [certificates](#certificate-soundness),
  [applying](#applying-a-certificate), [catch-up](#catch-up-and-recovery)
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
one. This is the core of the design, and it also closes a regression the design would otherwise introduce: the
existing receiver check permits acting on a member of *equal* role, so an owner-free group whose admins could remove
admins would let any one of them remove all the others. Owners are removal-proof today, so a design that abolishes
owners must put the admin set out of unilateral reach or it leaves a rogue admin more dangerous than before.

Any member may propose a new admin set. Members vote aye or nay with Ed25519 signatures over the proposal hash, sent
over their direct connections. Anyone can assemble the votes into a **certificate**, a self-authenticating proof that
the proposal passed, which every member validates independently and applies atomically under a monotonic governance
version. The tally is a **simple majority of votes cast** (`A > B`), paired with a duration that falls as support
rises: a certificate may be evaluated only after `maxReferendumDays × max(0, 1 − 2A/E)`, counted from when that member
first saw the proposal. Weak support waits; a majority of the whole electorate is ripe at once.

Incumbents cannot block a referendum in progress. Governance events are self-authenticating and forwardable by any
member, a removed member keeps its vote for a referendum period, and certificates need no admin cooperation to be
assembled, transported or applied. What incumbents can still do *before* a referendum exists is set out under
limitations.

## Design

### Scope (v1)

- p2p groups only (`useRelays = false`). Relay groups and channels are future work: they are single-owner today, and
  replacing their owner set needs changes to the short-link owner chain in simplexmq.
- One referendum action: replace the set of `GRAdmin` members. Moderator and member roles are untouched. The action is
  a sum type, so profile changes, parameter changes and group deletion can be added without protocol redesign.
- Groups of roughly 5 to 50 members, degrading past 100. Below five, referenda are ceremony over conversation.

### Prerequisite: member signing keys in p2p groups

Channels already give every member a per-group Ed25519 key (`GroupMember.memberPubKey`), and the wire format already
distributes keys in p2p groups: `MemberInfo` carries `memberKey`, populated by `memberInfo`, and travels in
`XGrpMemNew`, `XGrpMemIntro` and `XGrpMemFwd`. The verification path exists too, since `withVerifiedMsg` verifies
`CBGroup` signatures with prefix `smpEncode chatBinding <> smpEncode (memberId, pubKey)`.

Missing: generating and persisting a per-group keypair, populating `memberPubKey` on join and introduction, and
TOFU-pinning received keys as `applyMemberKeyRole` does for channels. Existing members announce their key on upgrade,
and governance cannot be enabled until every member's key is known.

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
`smpEncode ("SXGG", governanceId, params, memberSetHash)`. Receivers validate, fail-closed on any failure, that:

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

From here no member holds `GROwner`, and governed clients reject any event that would create or set one, so `XGrpDel`
has no valid sender and the "delete the group to pre-empt a vote" attack is closed. Owner-gated checks for `XGrpInfo`
and `XGrpPrefs` relax to `GRAdmin` on both sides, since an owner-free group could otherwise never edit its profile
again. For new groups the creator enables at creation, a self-signed genesis over a membership of one.

### Electorate

> The **electorate** is the set of current members the receiver records, at any role and including blocked members,
> plus those it has removed within the last `maxReferendumDays`. A member may vote if and only if it is in that set,
> and `E` is its size.

Throughout: `A` is valid aye votes, `B` valid nays, `T = A + B` turnout, `E` the electorate size.

One set decides everything: who may vote, the denominator of the thresholds, who may propose, and who an action may
name. Eligibility deliberately ignores role and `blockedByAdmin`, since both are levers incumbents control.

**Recently removed members still vote.** Because `E` does not appear in the pass condition `A > B`, a removal that
only shrank the denominator would *lower* the bar a purger has to clear. Keeping the removed in the vote as well as
the count is what makes a purge futile for one referendum period and gives the group that period to answer with a
referendum the purged can vote in. Recency is measured by the receiver's own record of the removal, never by anything
an author writes.

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
  , action :: GovAction               -- GAReplaceAdmins [MemberId]: sorted, non-empty,
                                      -- every named member in the receiver's electorate
  , prevProposalHash :: ByteString    -- proposal applied at the previous version,
                                      -- or the genesis hash at govVersion = 2
  , proposer :: MemberId, sig :: Signature }
  -- proposalHash = sha256 (smpEncode ("SXGP", governanceId, govVersion, action,
  --                                   prevProposalHash, proposer))

XGrpGovVote
  { governanceId, proposalHash, voter :: MemberId, vote :: Aye | Nay, sig }
  -- sig over smpEncode ("SXGV", governanceId, proposalHash, voter, vote).
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

Conflicting signed votes from one member on one proposal annul that member's vote, which keeps vote-set union
order-independent. Signatures are detached and over deterministic encodings so third parties can re-aggregate them
into certificates; this differs from shipped message signing, which signs transmitted bytes in an envelope. At ~100
bytes per vote, groups beyond ~120 members need the chunked-blob transport already used for roster blobs.

### Transport rules

1. All `x.grp.gov.*` events are added to `isForwardedGroupMsg` and exempt from `expectedForwarder` and the admin-only
   forwarder check: they are self-authenticating, so any member may forward or rebroadcast them, with the existing
   `sharedMsgId` dedup. An admin dropping them achieves nothing while any other path exists.
2. Governance events are exempt from `blockedByAdmin` forwarding suppression.
3. Demotion and blocking never affect voting, since eligibility depends only on membership.
4. **Removal deferral.** While a client holds an unresolved proposal it defers the connection teardown that
   `XGrpMemDel` normally triggers, for electorate members, and exempts `x.grp.gov.*` from the removed-member send
   guard, so a removed member stays reachable and can still vote. Deferral ends at the earlier of local resolution
   plus one challenge window, or `firstSeenAt + maxReferendumDays +` the maximum window. Its job is only reachability:
   the recency rule already preserves the vote, and unlike deferral it does not depend on which message a client
   happens to hold first.

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
   requires **attestations**: `max(3, ⌈E/10⌉)` distinct members, outside the certificate's aye set and reached by a
   path the presenter does not control, must have signed `smpEncode ("SXGS", governanceId, govVersion, proposalHash)`
   for *that proposal*. Comparing versions alone would let honest attestations for the legitimate certificate at a
   version authorise a fabricated rival at the same version. The threshold scales with the group, because a fixed
   count of three is within reach of a small conspiracy however large the group is;
7. applies atomically: set every member named in the action to `GRAdmin`, demote every other current `GRAdmin` to
   `GRMember`, and store the version, proposal and certificate;
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

*cannot:* change the admin set outside a referendum, so it cannot remove its colleagues, pack the set, or entrench
itself; forge a certificate, since admins do not hold members' keys; veto one, since no admin signature appears in
propose, vote or apply; censor one, since governance events are forwardable by any member; silence a voter by
demotion, blocking or removal, since the removed keep their vote for a referendum period; rush a result, since speed
must be bought with support and no timestamp a proposer writes is read by any rule.

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

*cannot:* produce an unconditional certificate without a genuine majority of the receiver's electorate; satisfy the
attestation bound with its own supporters, or with attestations for a different proposal.

## Limitations

- **The electorate is admin-curated.** Membership comes from what admins have told each member, so an admin can seed
  the electorate with identities that do not exist, or keep real members out of it, before any referendum runs. This
  is the design's largest weakness and it is not solved here. Douceur's result rules out solving it outright without a
  central authority; partial mitigations are future work.
- **Enabling is the moment of maximum exposure.** Genesis is validated against local knowledge, so an owner that has
  equivocated membership beforehand can seal a skewed electorate, and the ownerless founding certificate is judged
  against the same list. Enable governance in a group whose membership is visibly settled.
- **A purge outside a live referendum still works, slowly.** Recency protects a purged member's vote for one
  referendum period; after that the removals stand. It buys the group `maxReferendumDays` to respond, not immunity.
- **Removal deferral freezes moderation.** While a referendum is live, removals of electorate members do not sever
  connections, and successive proposers can chain referenda to extend that. Deferred connections carry only governance
  events, but this is a real cost and wants a per-peer budget at implementation time.
- **Governance traffic is unsuppressible.** The exemptions that make censorship hard also mean no moderation lever
  closes that channel: a blocked or removed member can keep sending valid governance events, and can fill a targeted
  member's queue toward the 128-message quota.
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

- `Protocol.hs`: `GovAction`, the five event types and tags (added to `isForwardedGroupMsg`), deterministic binary
  encodings with domain separation (`SXGG`/`SXGP`/`SXGV`/`SXGS`).
- **Prerequisite:** the `xGrpMemIntro` role cap, for all p2p groups rather than only governed ones.
- Key management: per-group member keypair for p2p governed groups, population of `memberPubKey` on join and
  introduction, TOFU pinning as in `applyMemberKeyRole`.
- Sign and verify `XGrpMemNew`/`XGrpMemDel`/`XGrpMemRole` in governed p2p groups through the existing p2p branch in
  `withVerifiedMsg`, which independently closes the unsigned-forward forgery hole.
- `Subscriber.hs`: handlers for the five events; genesis validation (checks 1 to 4); proposal validation including
  `prevProposalHash` on every path; ripeness from a persisted per-proposal `first_seen_at`, with `E` as a per-proposal
  running maximum; the challenge-window worker with late voting; the apply procedure above; catch-up serving with
  per-requester bounds; forwarder and `blockedByAdmin` exemptions; removal deferral; **rejection of any admin-set
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
  status             TEXT    NOT NULL,  -- active / passed / failed / superseded / witnessed
  applied_cert       BLOB,              -- as-applied vote set, re-served on catch-up
  attestations       BLOB
);
CREATE TABLE group_referendum_votes (
  referendum_id   INTEGER NOT NULL REFERENCES group_referenda ON DELETE CASCADE,
  group_member_id INTEGER NOT NULL,
  vote            TEXT    NOT NULL,
  vote_sig        BLOB    NOT NULL,
  annulled        INTEGER NOT NULL DEFAULT 0,
  UNIQUE (referendum_id, group_member_id)
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
- **Reputation-weighted or Sybil-resistant voting**, and **ballot secrecy** for groups that need it.

## Related work

Full analysis in [`2026-08-01-group-governance-related-work.md`](2026-08-01-group-governance-related-work.md). In
brief: replacing an admin set with *revocation* semantics has consensus number *N* and is unattainable here, which is
why finality is local and revisable rather than a shortcoming to be engineered away ([Frey, Gestin &
Raynal](https://arxiv.org/abs/2302.06344), DISC 2023; [FLP](https://doi.org/10.1145/3149.214121)). Vote accumulation is
I-confluent and the winner is decided by a deterministic order rather than by consensus, following [Kleppmann &
Howard](https://arxiv.org/abs/2012.00472). The closest published work on concurrent admin changes,
[ERA](https://arxiv.org/abs/2601.22963) (PaPoC '26), concludes that a trusted arbiter is required; we decline one for
the same reason we are removing admin chokepoints, and accept weaker finality instead. Its critique of seniority
ranking applies directly to SimpleX's existing `roleRequiredToChange`.
[MLS](https://www.rfc-editor.org/rfc/rfc9420.html) serializes concurrent commits at its Delivery Service, which we do
not have; [DCGKA](https://doi.org/10.1145/3460120.3484542) (CCS 2021) is serverless, tolerates the same membership
flickering, and scopes authorization policy out, which is the gap this fills. The tally is plain majority per May's
theorem; we reject turnout quorums for their abstention incentive, and Polkadot's adaptive quorum biasing because its
required support is scale-invariant in the opposition share, which suits a population with standing monitoring
delegates rather than a private group. [Douceur](https://doi.org/10.1007/3-540-45748-8_24) bounds what the electorate
limitation can ever become.
