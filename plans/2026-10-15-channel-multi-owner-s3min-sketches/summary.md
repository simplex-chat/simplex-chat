# Channel multi-owner (S3-min): summary

Several equal owners per channel, each owner change ordered by the link server; nothing is implemented yet. S3-min is the plan's name for this design: the minimal one that keeps every feature of the original, larger design.

**Problem.** Today a channel's creator is its only owner: it overwrites the link data on the link server with `LSET`, last writer wins, and alone numbers the roster, the signed list of member, moderator and admin roles that relays and members apply in version order [§3]. Several owners writing this way would undo each other's changes.

**Idea.** The link server already orders writes to the link, so each owner change becomes a compare-and-swap: an owner reads the link data, applies its one change and writes it back with a new command, `LSWP`, carrying the hash of what it read. If another owner wrote in between, the server answers `CONFLICT`, and the owner reads again and re-applies, five attempts at most [§4.1, §4.4].

![Two owners writing at once](2-owner-write.svg)

Sources: the [overview](../2026-10-15-channel-multi-owner-s3min.md) and the [implementation plan](../2026-10-15-channel-multi-owner-s3min-implementation.md). Brackets cite them: [Ov: heading] is a heading of the overview, [§4.4] a section of the implementation plan; T8 and T17 are its tests (§7).

Terms: the link is the channel's link data on the link server; its write counter, `linkVersion`, goes up with every swap; the roster version numbers role changes; a link sync is an owner's background work on the link, defined below.

## Approach and why

A member becomes an owner only by accepting; owners can remove each other, and any owner, the creator included, can leave. Owners are trusted as the single owner is today, and the design defends against malicious relays and a link server that withholds or serves stale data. So relays handle owner actions as today, without checking them against the link, and relays and members keep today's roster protocol [Ov: intro, Ov: Decisions, §4]. Work an owner does in the background, such as finishing a promotion, is a **link sync**: agent commands queued on the owner's connection to a relay, each step re-derived from the latest read, so it survives restarts [§4.4].

![Who holds what](1-components.svg)

## Adding and removing owners

![Adding an owner](4-promotion.svg)

Transferring ownership is this promotion, then the creator leaves.

![Removing an owner, and leaving](5-removal-and-leave.svg)

## How owners, relays and members stay consistent

- **Reads** ignore link data older than the stored `linkVersion` and only add owners or mark revoked ones departed: a stale read can't undo a promotion, but can make a party take a removed owner as current until its next read; that device can no longer write the link, and an honest one sends nothing [§4.4, §5].
- **Roster**: an owner may claim the next roster version on the link only while it holds the complete roster at the current one (picture below). An owner that sees another owner's role change above its version fetches the roster from the relays; roster messages from an owner who has since left or been removed are still applied in version order; an undelivered own roster is re-signed at the next version [§4.10].
- **Relays** accept from a departed owner only its leave and roster messages up to the link's roster version. Relays and members skip an `x.grp.info` older than their `linkVersion`; relays still forward it [§4.12, §4.13].
- **Link syncs** run in the main app only, at most five swaps per channel per trigger. The first current owner in the chain publishes the subscriber count, batching joins over a minute [§4.4, §4.11, §4.15].

![Two owners changing roles](3-roster-claims.svg)

## Trade-offs and residual risks

Trade-offs [Ov: Trade-offs users notice, Ov: Decisions]:

- Owner actions need the link server, posting doesn't; a role change takes 1–3 s and can be refused for a few seconds after another owner's.
- The link server learns when owners act and how many there are; a new owner's profile reaches every subscriber, and its member list comes from one relay, without profile pictures (a malicious relay can add made-up subscribers to it, never members with roles).
- The link orders profile, role, owner and relay changes, leaving and destroying, each announced after its write lands; posts, moderation, blocks, subscriber removals and invitations are sent as today, outside the link [§5]. Not built: cancelling an invitation, merging two profile edits field by field, a tie-break between equal roster versions, retiring the creator's root key.
- Older apps and servers: promotion needs every relay on chat protocol v23 and the link server on SMP v24. Older members reject a new owner's messages until they upgrade; older joiners take every owner the link lists as an owner, removed ones included, though those keys can't write the link. A channel's link writes switch to the swap at its first accepted invitation and stay as today until then; a single-owner creator also reads the link before leaving or destroying, and a failed read refuses the leave [§4.3, §4.9, §4.14, §5].
- A promotion wins over a concurrent removal or leave of member M (decided 2026-10-19 as a rare case, with no mitigation). The link decides who is listed as an owner; what each party does depends on which message it applied first [§4.8]:

| Party | Applied the removal first | Applied the commit first |
|---|---|---|
| Relays, members, owners | refuse the commit; take M as an owner at the next read; relays have dropped their connection to M | M is an owner; an admin's later removal is refused, an owner's is applied and the next read makes M an owner again |
| M's own device | has left the channel; the link still lists M as an owner, with no device behind it, until an owner removes it | admin remover: M stays an owner; owner remover: applies the removal and leaves |

The remover's device always applied the removal first. The owner-remover cells follow today's `checkRole`, which the plan keeps; test T8 covers only the removal-first order.

Residual risks [Ov: Residual risks, §5]:

- A link server ignoring the compare-and-swap can lose writes or split roles, but can't forge.
- A removed owner on modified software acts as an owner through a relay until the relay learns of the removal (the announcement, or its next 15-minute check), including inflated role versions that block later role changes there and at its members. A relay that learned from its check still accepts its new role events up to the link's roster version, and members apply them; with a colluding link server the removed owner can serve forged link data.
- Any current owner can destroy the channel or lock the others out.
- A role change no relay serves (author lost or removed first, or its only relay failed) blocks other owners' role changes while a lost author is still an owner. Once it is removed, the others wait up to a day, then one owner skips its version and carries on from its own roster, which every device then takes: role changes from that version that some devices had already applied are undone there, and so are that author's earlier changes the skipping owner never received, wherever they were applied.
- A remover crashing after its swap sends no announcement; others learn at their next read.
- A creator restored from a backup older than its first promotion acts as the single owner until it leaves or destroys, and its role changes can collide. A downgraded server keeps the owner keys but loses link data written since its last start, stops multi-owner writes and accepts `LSET` again; on an upgraded server a creator on an older app can't change the link.

## Slices

~2,260 production Haskell lines (simplexmq ~490 plus chat ~1,770) plus tests, ~380 per app platform and ~850 of CI scripts, scripted checks and test plans; sixteen slices, about 110 hours; each compiles and passes its tests alone, and the app slices are built and tested on GitHub's runners [§6, §7.1].

| Slices | What |
|---|---|
| S1–S2 | simplexmq: store and swap, SMP protocol v24, agent |
| S3–S5 | chat: wire and data, link writes and reads, link syncs, profile, relays, count |
| S6 | CI: a test backend with channels on a v24 link server |
| S7–S8 | promotion, member catch-up; guards |
| S9–S10 | roster through the link |
| S11–S12 | apps: owner invitation and promotion, iOS then Kotlin |
| S13–S14 | leave, removal, destroy |
| S15–S16 | apps: owner management, iOS then Kotlin |

**Risks** [§8, Ov: Not verified, size]: the riskiest assumption, which tests in S9, S10 and S13 check, is that today's roster protocol converges when versions are claimed through the link from complete rosters and departed owners' roster messages are still applied. The next is that background callbacks, re-derived from the latest read and the database, are idempotent; one that isn't would send twice or never (test T17, in S8). S1 first proves the swap's atomicity and the computed 50-owner size; the 100,000-member catch-up is an estimate. Postgres is covered by the Haskell tests, not by the app runs.

**Open in plan §9**, the two design questions, taken as recommended unless a review shows otherwise, and built in S1: `LSWP` sending only the user data, which leaves room for about 50 owner keys (computed from the encoders, not yet tested) instead of 6, and refusing `LSET` on multi-key queues, a server behaviour change that stops stale or downgraded single-owner writes from erasing other owners. A third is for S1: the server design was written against an older simplexmq, which has since dropped its journal store and Postgres queue cache, so S1 builds the swap for the two stores left and updates the design's code references.

The rest of the page is reference for the programmers who build it.

## Protocol changes

| Layer | Change | Plan |
|---|---|---|
| SMP v24 | `LSWP hash [keys] userData`: stores the user data, and the key set if given, only if `hash` is the SHA-256 of the stored user data; not proxied | §4.1 |
| SMP v24 | `CONFLICT`; `LSET` refused (`AUTH`) on multi-key queues; store-log record `LKEYS` (link data with its keys) | §4.1 |
| Link data | optional `linkVersion`, `revokedOwners`, `roster {version, author}`, `creatorQueueKey` and `linkRcvId` (the creator's queue key, and the queue address other owners write to) | §4.3 |
| Chat protocol v23 | `x.grp.owner.inv`, `x.grp.owner.acpt`, `x.grp.owner.add` (invite, consent, commit); `x.grp.members.req`, `x.grp.members.page` (member list); `linkVersion` in `x.grp.info` | §4.5 |
## API, commands and apps

- **`/owner invite #<group> <member>`** (`APIInviteGroupOwner`) needs the member's security code verified (only this catches a relay substituting the member's key), relays on chat v23, the link server on SMP v24 and a chain under 50 entries [§4.6].
- **`/_owner accept #<groupId> <chatItemId> [direct=on]`** (`APIAcceptGroupOwnerInvitation`): the device then talks to the link server directly, revealing its IP address; private routing asks first, and settings forbidding direct connections refuse [§4.6].
- **In multi-owner mode** role changes, and removals of members, moderators and admins, claim the next roster version on the link; removing an owner and leaving write the link first; destroying deletes the link queue first. A profile edit is refused if the link's profile differs from the device's at the edit's start; the apps also refuse a save over a profile changed while editing [§4.8–§4.14].
- **Apps**, iOS then Kotlin, ~380 lines each: Owner in the role picker via verification, the invitation, Remove owner, Leave, and the link and relay screens for every owner. Group-link commands are refused in channels [§4.14, §6.1].

## Types by module

| Module | Added or changed | Plan |
|---|---|---|
| simplexmq `Protocol`, `Server`, stores, `Client` | `LinkDataHash`, `CONFLICT`, `LSWP`, `swapQueueLinkData`, `CreateLinkKeys`; SMP version 23 → 24 | §4.1 |
| simplexmq agent | `LinkOwnerCreds`, `setShortLinkSwap` (sync, async), `deleteLinkQueueWithCreds`, hash in `LDATA` | §4.2 |
| chat `Protocol`, `Controller`, `Types` | link fields, events, chat protocol 22 → 23; two commands, `CEChannelLink`, `CFChannelLinkSync`, `CFChannelLinkResign`, `CFChannelLinkSwap` | §4.3, §4.5, §4.16 |
| chat `Library/*` | `writeChannelLink`, `enqueueLinkSync`, `applyChannelLinkData`, `claimRosterVersion` | §4.4, §4.10 |

## Database

`groups` gains `link_data_version`, `link_roster_version`, `link_roster_seen_at`, `members_catchup` and `roster_pending_msg_id`; `group_members` gains `owner_inv_msg_id`, `owner_inv_accepted` and `relay_web_domain`. One migration, `M20261015_channel_multi_owner`, SQLite and Postgres, columns only; no agent or server schema change [§4.16, §5].
