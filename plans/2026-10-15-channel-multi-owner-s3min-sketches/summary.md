# Channels with several owners: summary

The plan lets a channel have several equal owners: the server that holds the channel's link takes their changes one at a time, so concurrent changes are never lost or undone. Nothing is built yet; please confirm the choices below.

![Two owners change the link at once](2-owner-write.svg)

`LSWP` is the new write command: it carries the hash (h1, h2) of the link data the owner read, and the server stores it only if that hash still matches. Otherwise the server answers `CONFLICT`, and the owner reads again and re-applies its change, so neither change is lost. Sources: [Overview, How it works][ov28]: the one new server command; [Plan §4.1][pl44-48]: the hash, `CONFLICT` and `LSWP`; [Plan §4.4][pl100-107]: read, apply, swap, at most five attempts, then the announcement.

![Who holds what](1-components.svg)

Only owners write the channel's link; relays and members read it, and relays carry everything else between owners and members, as today. Blue marks what the plan adds. Sources: [Overview, How it works][ov26-32]: the link server orders what owners share; [Plan §4.1][pl45-48]: the key set swapped with the data; [Plan §4.3][pl80-86]: the new link-data fields; [Plan §4.4, step 3][pl104]: which keys may write; [Plan §4.4][pl135]: who reads the link, and when; [Plan §4.7][pl188-190]: member pages for a new owner; [Plan §4.12][pl247]: older profile updates skipped; [Plan §5][pl313-317]: relays can't forge owners; the 15-minute relay check; writes direct, reads private.

![Adding an owner](4-promotion.svg)

A member becomes an owner only after its security code is verified and it accepts; one link write adds it. The messages, in order: the invitation, the acceptance, the announcement to everyone, then the new owner's download of the member list from a relay. Transferring ownership is this promotion, then the creator leaves. Sources: [Overview, What owners can do][ov14]: make a member an owner; [Plan §4.6][pl163-184]: invite, accept, the link write, the commit; [Plan §4.7][pl188-190]: the new owner's member list; [Plan §6.1][pl373]: the "Become an owner?" question; [Plan §7, T3][pl399]: the creator promotes M and leaves.

![Removing an owner, and leaving](5-removal-and-leave.svg)

A removal or a leave lands on the link first: from then the server refuses the departed owner's key (`AUTH`), and the announcement follows. The last owner can't leave. Sources: [Overview, What owners can do][ov16-17]: remove an owner, leave; [Plan §4.8][pl194-198]: removing an owner; [Plan §4.9][pl202-208]: leaving, with the re-sign and the 30-second wait; [Plan §1, AC4][pl16]: the removed owner's writes and `DEL` get `AUTH`.

![Two owners change roles](3-roster-claims.svg)

The roster is the signed list of member, moderator and admin roles, and 7, 8 and 9 are its versions. An owner takes the next version only while it holds every earlier one, so members and relays apply role changes in order, as today; an owner that is behind sees "catching up, try again" for a few seconds. Sources: [Overview, How it works][ov29]: role changes numbered on the link server; [Overview, What owners can do][ov15] and [plan §4.10][pl221]: the roster's roles, owners outside it; [Overview, Trade-offs users notice][ov49]: "catching up, try again"; [Plan §4.10][pl212-219]: claims from the complete roster, and the roster request; [Plan §4.17][pl297]: two role changes at once.

- **For users.** Today a channel has one owner, its creator. With the plan any owner can post, edit the channel, change roles, manage relays and destroy the channel; a member becomes an owner only after its security code is verified and it accepts an invitation; owners can remove each other, and the channel keeps working after the creator leaves. [overview][ov3-22]
- **How.** An owner reads the channel's link, makes its one change and writes it back; the server takes the write only if nobody wrote since that read, otherwise the owner reads again and re-applies. Role changes are numbered on the same server, so relays and members keep today's role protocol. [overview][ov24-32]
- **What users notice.** Owner actions need that server, posting doesn't; a role change takes 1–3 seconds and can be refused for a few seconds after another owner's ("catching up, try again"); a new owner's device connects to the server directly, showing it the device's IP address, and the invitation says so. [overview][ov47-55], [timing][ov38]
- **Trust.** Owners trust each other as the single owner is trusted today: any owner can destroy the channel or lock the others out. The design guards against dishonest relays and a server that withholds the link or serves an old copy. [overview][ov3], [risks][ov57-63]
- **Cost and order.** About 110 hours: the server and the messaging library first, then the chat core, with the iOS, Android and desktop apps after the core parts they need, built and tested on GitHub's runners. The riskiest part, role changes by several owners reaching every member in order, is mostly tested before any app work. [plan][pl319-342], [risk][pl458]

## Questions for you

The first slice builds these as recommended unless you choose otherwise. On the published page, https://claude.ai/artifact/25Z9m6xPzLMkiGNJ6TeKgy, each takes an answer and a comment.

1. **How much should an owner's write send to the server?**
   - Recommended: **only the part owners change, as planned.** Room for up to about 50 owners, since the write fits 52 owner keys; a write shaped like today's fits 6. The figure is computed from the encoders and the first slice tests it; the owner list shares the link data's fixed size with the channel's profile, so a large profile picture leaves room for fewer (not yet measured). Source: [plan §9, question 4][pl468], [§4.1][pl45], [overview][ov72], [the link data's size in simplexmq][smq-ud-size].
   - **The whole link data, as today's write.** Owners capped at about 6. Source: [plan §9, question 4][pl468].
2. **Should the server refuse today's write command on a link that several owners write?**
   - Recommended: **refuse it, as planned.** It stops a stale write, or one from a downgraded app, from erasing the other owners' changes. Single-owner channels are unaffected: their links have one owner key. It changes the server's behaviour in its new version. Source: [plan §9, question 5][pl469], [§4.1][pl47].
   - **Keep accepting it.** Nothing then stops a stale or downgraded single-owner write from erasing the other owners' changes. Source: [plan §9, question 5][pl469].

## For programmers

The rest of the page is reference for the programmers who build it. S3-min is the plan's name for this design: the minimal one that keeps the features of the original, larger design except four optional parts, listed under Trade-offs.

### Problem, idea and approach

**Problem.** Today a channel's creator is its only owner: it overwrites the link data on the link server with `LSET`, last writer wins, and alone numbers the roster, the signed list of member, moderator and admin roles that relays and members apply in version order [[§3][pl31-35], [§4.10][pl221]]. Several owners writing this way would undo each other's changes.

**Idea.** The link server already orders writes to the link, so each owner change becomes a compare-and-swap: an owner reads the link data, applies its one change and writes it back with a new command, `LSWP`, carrying the hash of what it read. If another owner wrote in between, the server answers `CONFLICT`, and the owner reads again and re-applies, five attempts at most [[§4.1][pl44-46], [§4.4][pl105-106]].

**Approach.** A member becomes an owner only by accepting; owners can remove each other, and any owner, the creator included, can leave. Owners are trusted as the single owner is today, and the design defends against malicious relays and a link server that withholds or serves stale data. So relays handle owner actions as today, without checking them against the link, and relays and members keep today's roster protocol [[Ov: intro][ov3], [Ov: Decisions][ov43], [§4][pl39]]. Work an owner does in the background, such as finishing a promotion, is a **link sync**: agent commands queued on the owner's connection to a relay, each step re-derived from the latest read, so it survives restarts [[§4.4][pl112-118]].

Terms: the link is the channel's link data on the link server; its write counter, `linkVersion`, goes up with every swap; the roster version numbers role changes; a link sync is an owner's background work on the link, defined above.

Sources: the [overview](../2026-10-15-channel-multi-owner-s3min.md) and the [implementation plan](../2026-10-15-channel-multi-owner-s3min-implementation.md). Each bracket links to the lines it cites, at the commit this page describes: [Ov: heading] in the overview, [§4.4] in a section of the implementation plan; T8 and T17 are its tests (§7).

### How owners, relays and members stay consistent

- **Reads** ignore link data older than the stored `linkVersion` and only add owners or mark revoked ones departed: a stale read can't undo a promotion, but can make a party take a removed owner as current until its next read; that device can no longer write the link, and an honest one sends nothing [[§4.4][pl137-140], [§5][pl314]].
- **Roster**: an owner may claim the next roster version on the link only while it holds the complete roster at the current one (the picture "Two owners change roles"). An owner that sees another owner's role change above its version fetches the roster from the relays; roster messages from an owner who has since left or been removed are still applied in version order; an undelivered own roster is re-signed at the next version [[§4.10][pl210-237]].
- **Relays** accept from a departed owner only its leave and roster messages up to the link's roster version. Relays and members skip an `x.grp.info` older than their `linkVersion`; relays still forward it [[§4.12][pl247], [§4.13][pl252]].
- **Link syncs** run in the main app only, at most five swaps per channel per trigger. The first current owner in the chain publishes the subscriber count, batching joins over a minute [[§4.4][pl112-128], [§4.11][pl241], [§4.15][pl272]].

### Trade-offs

Trade-offs [[Ov: Trade-offs users notice][ov47-55], [Ov: Decisions][ov34-45]]:

- Owner actions need the link server, posting doesn't; a role change takes 1–3 s and can be refused for a few seconds after another owner's.
- The link server learns when owners act and how many there are; a new owner's profile reaches every subscriber, and its member list comes from one relay, without profile pictures (a malicious relay can add made-up subscribers to it, never members with roles [[§5][pl313]]).
- The link orders profile, role, owner and relay changes, leaving and destroying, each announced after its write lands; posts, moderation, blocks, subscriber removals and invitations are sent as today, outside the link [[§5][pl315]]. Not built: cancelling an invitation, merging two profile edits field by field, a tie-break between equal roster versions, retiring the creator's root key.
- Older apps and servers: promotion needs every relay on chat protocol v23 and the link server on SMP v24. Older members reject a new owner's messages until they upgrade; older joiners take every owner the link lists as an owner, removed ones included, though those keys can't write the link. A channel's link writes switch to the swap at its first accepted invitation and stay as today until then; a single-owner creator also reads the link before leaving or destroying, and a failed read refuses the leave [[§4.3][pl92], [§4.9][pl202], [§4.14][pl268], [§5][pl311]].
- A promotion wins over a concurrent removal or leave of member M (decided, with nothing done for it). The link decides who is listed as an owner; what each party does depends on which message it applied first [[§4.8][pl197]]:

| Party | Applied the removal first | Applied the commit first |
|---|---|---|
| Relays, members, owners | refuse the commit; take M as an owner at the next read; relays have dropped their connection to M | M is an owner; an admin's later removal is refused, an owner's is applied and the next read makes M an owner again |
| M's own device | has left the channel; the link still lists M as an owner, with no device behind it, until an owner removes it | admin remover: M stays an owner; owner remover: applies the removal and leaves |

The remover's device always applied the removal first. The owner-remover cells follow today's `checkRole`, which the plan keeps; test [T8][pl404] covers only the removal-first order.

### Residual risks

Residual risks [[Ov: Residual risks][ov57-68], [§5][pl310-316]]:

- A link server ignoring the compare-and-swap can lose writes or split roles, but can't forge.
- A removed owner on modified software acts as an owner through a relay until the relay learns of the removal (the announcement, or its next 15-minute check), including inflated role versions that block later role changes there and at its members. A relay that learned from its check still accepts its new role events up to the link's roster version, and members apply them; with a colluding link server the removed owner can serve forged link data.
- Any current owner can destroy the channel or lock the others out.
- A role change no relay serves (author lost or removed first, or its only relay failed) blocks other owners' role changes while a lost author is still an owner. Once it is removed, the others wait up to a day, then one owner skips its version and carries on from its own roster, which every device then takes: role changes from that version that some devices had already applied are undone there, and so are that author's earlier changes the skipping owner never received, wherever they were applied.
- A remover crashing after its swap sends no announcement; others learn at their next read.
- A creator restored from a backup older than its first promotion acts as the single owner until it leaves or destroys, and its role changes can collide. A downgraded server keeps the owner keys but can lose the data of owner-changing writes since its last start, stops multi-owner writes and accepts `LSET` again; on an upgraded server a creator on an older app can't change the link.

### Slices, size and risks

~2,260 production Haskell lines (simplexmq ~490 plus chat ~1,770) plus tests, ~380 per app platform and ~850 of CI scripts, scripted checks and test plans; sixteen slices, about 110 hours; each compiles and passes its tests alone, and the app slices are built and tested on GitHub's runners [[header][pl3], [§6][pl319-342], [§7.1][pl424]].

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

**Risks** [[§8][pl456-459], [Ov: Not verified, size][ov70-72]]: the riskiest assumption, which tests in S9, S10 and S13 check, is that today's roster protocol converges when versions are claimed through the link from complete rosters and departed owners' roster messages are still applied. The next is that background callbacks, re-derived from the latest read and the database, are idempotent; one that isn't would send twice or never (test [T17][pl413], in S8). S1 first proves the swap's atomicity and the computed 50-owner size; the 100,000-member catch-up is an estimate. Postgres is covered by the Haskell tests, not by the app runs.

**Open in plan [§9][pl464-471]**, asked under "Questions for you": the two design questions, which S1 builds as recommended unless you choose otherwise: `LSWP` sending only the user data, which leaves room for about 50 owner keys (computed from the encoders, not yet tested) instead of 6 [[§4.1][pl45], [Ov: Not verified, size][ov72]], and refusing `LSET` on multi-key queues, a server behaviour change that stops stale or downgraded single-owner writes from erasing other owners. A third, decided on 26 Oct: the server design was written against an older simplexmq, which has since dropped its journal store and Postgres queue cache, so S1 builds the swap for the two stores left and updates the design's code references [[§9][pl471]].

### Protocol changes

| Layer | Change | Plan |
|---|---|---|
| SMP v24 | `LSWP hash [keys] userData`: stores the user data, and the key set if given, only if `hash` is the SHA-256 of the stored user data; not proxied | [§4.1][pl44-46] |
| SMP v24 | `CONFLICT`; `LSET` refused (`AUTH`) on multi-key queues; store-log record `LKEYS` (link data with its keys) | [§4.1][pl44-52] |
| Link data | optional `linkVersion`, `revokedOwners`, `roster {version, author}`, `creatorQueueKey` and `linkRcvId` (the creator's queue key, and the queue address other owners write to) | [§4.3][pl78-86] |
| Chat protocol v23 | `x.grp.owner.inv`, `x.grp.owner.acpt`, `x.grp.owner.add` (invite, consent, commit); `x.grp.members.req`, `x.grp.members.page` (member list); `linkVersion` in `x.grp.info` | [§4.5][pl148-157] |

### API, commands and apps

- **`/owner invite #<group> <member>`** (`APIInviteGroupOwner`) needs the member's security code verified (only this catches a relay substituting the member's key), relays on chat v23, the link server on SMP v24 and a chain under 50 entries [[§4.6][pl163-168]].
- **`/_owner accept #<groupId> <chatItemId> [direct=on]`** (`APIAcceptGroupOwnerInvitation`): the device then talks to the link server directly, revealing its IP address; private routing asks first, and settings forbidding direct connections refuse [[§4.6][pl174]].
- **In multi-owner mode** role changes, and removals of members, moderators and admins, claim the next roster version on the link; removing an owner and leaving write the link first; destroying deletes the link queue first. A profile edit is refused if the link's profile differs from the device's at the edit's start; the apps also refuse a save over a profile changed while editing [[§4.8–§4.14][pl192-268]].
- **Apps**, iOS then Kotlin, ~380 lines each: Owner in the role picker via verification, the invitation, Remove owner, Leave, and the link and relay screens for every owner. Group-link commands are refused in channels [[§4.14][pl264], [§6.1][pl365-382]].

### Types by module

| Module | Added or changed | Plan |
|---|---|---|
| simplexmq `Protocol`, `Server`, stores, `Client` | `LinkDataHash`, `CONFLICT`, `LSWP`, `swapQueueLinkData`, `CreateLinkKeys`; SMP version 23 → 24 | [§4.1][pl43-53] |
| simplexmq agent | `LinkOwnerCreds`, `setShortLinkSwap` (sync, async), `deleteLinkQueueWithCreds`, hash in `LDATA` | [§4.2][pl57-70] |
| chat `Protocol`, `Controller`, `Types` | link fields, events, chat protocol 22 → 23; two commands, `CEChannelLink`, `CFChannelLinkSync`, `CFChannelLinkResign`, `CFChannelLinkSwap` | [§4.3][pl78-86], [§4.5][pl148-157], [§4.16][pl276-286] |
| chat `Library/*` | `writeChannelLink`, `enqueueLinkSync`, `applyChannelLinkData`, `claimRosterVersion` | [§4.4][pl100-135], [§4.10][pl212] |

### Database

`groups` gains `link_data_version`, `link_roster_version`, `link_roster_seen_at`, `members_catchup` and `roster_pending_msg_id`; `group_members` gains `owner_inv_msg_id`, `owner_inv_accepted` and `relay_web_domain`. One migration (`M20261015_channel_multi_owner` in the design; S3 dates it when it adds it [[§6][pl350]]), SQLite and Postgres, columns only; no agent or server schema change [[§4.16][pl282-288], [§5][pl309]].

[ov3]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min.md?plain=1#L3
[ov3-22]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min.md?plain=1#L3-L22
[ov14]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min.md?plain=1#L14
[ov15]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min.md?plain=1#L15
[ov16-17]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min.md?plain=1#L16-L17
[ov24-32]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min.md?plain=1#L24-L32
[ov26-32]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min.md?plain=1#L26-L32
[ov28]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min.md?plain=1#L28
[ov29]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min.md?plain=1#L29
[ov34-45]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min.md?plain=1#L34-L45
[ov38]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min.md?plain=1#L38
[ov43]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min.md?plain=1#L43
[ov47-55]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min.md?plain=1#L47-L55
[ov49]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min.md?plain=1#L49
[ov57-63]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min.md?plain=1#L57-L63
[ov57-68]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min.md?plain=1#L57-L68
[ov70-72]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min.md?plain=1#L70-L72
[ov72]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min.md?plain=1#L72
[pl3]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L3
[pl16]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L16
[pl31-35]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L31-L35
[pl39]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L39
[pl43-53]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L43-L53
[pl44-46]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L44-L46
[pl44-48]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L44-L48
[pl44-52]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L44-L52
[pl45]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L45
[pl45-48]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L45-L48
[pl47]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L47
[pl57-70]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L57-L70
[pl78-86]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L78-L86
[pl80-86]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L80-L86
[pl92]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L92
[pl100-107]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L100-L107
[pl100-135]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L100-L135
[pl104]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L104
[pl105-106]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L105-L106
[pl112-118]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L112-L118
[pl112-128]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L112-L128
[pl135]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L135
[pl137-140]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L137-L140
[pl148-157]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L148-L157
[pl163-168]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L163-L168
[pl163-184]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L163-L184
[pl174]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L174
[pl188-190]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L188-L190
[pl192-268]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L192-L268
[pl194-198]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L194-L198
[pl197]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L197
[pl202]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L202
[pl202-208]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L202-L208
[pl210-237]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L210-L237
[pl212]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L212
[pl212-219]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L212-L219
[pl221]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L221
[pl241]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L241
[pl247]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L247
[pl252]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L252
[pl264]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L264
[pl268]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L268
[pl272]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L272
[pl276-286]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L276-L286
[pl282-288]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L282-L288
[pl297]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L297
[pl309]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L309
[pl310-316]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L310-L316
[pl311]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L311
[pl313-317]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L313-L317
[pl313]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L313
[pl314]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L314
[pl315]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L315
[pl319-342]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L319-L342
[pl350]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L350
[pl365-382]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L365-L382
[pl373]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L373
[pl399]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L399
[pl404]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L404
[pl413]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L413
[pl424]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L424
[pl456-459]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L456-L459
[pl458]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L458
[pl464-471]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L464-L471
[pl468]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L468
[pl469]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L469
[pl471]: https://github.com/spaced4ndy/simplex-chat/blob/8ccf8b8c873d81b57ff4283b95139f33c42a3848/plans/2026-10-15-channel-multi-owner-s3min-implementation.md?plain=1#L471
[smq-ud-size]: https://github.com/simplex-chat/simplexmq/blob/8bc4d3601250d9f79dc7e80ead319ec2b6d4a95a/src/Simplex/Messaging/Crypto/ShortLink.hs#L45-L46
