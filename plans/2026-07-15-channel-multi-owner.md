# Channel multi-owner

A channel can have several equal owners, per `docs/protocol/channels-overview.md` §Governance ("v7: any-owner-decides"). Any owner can take any administrative action without coordinating with the others: post, moderate, change the profile, manage relays, update the link data, add owners, destroy the channel. The channel keeps working after any owner, including its creator, leaves. Owners cannot remove one another in this version (§11).

simplexmq already models most of this and leaves it unwired:

- `UserContactData.owners :: [OwnerAuth]` is a signed chain: each entry is signed by the root key or an earlier entry (`validateLinkOwners`, `Agent/Protocol.hs:2003-2017`).
- Readers accept user data signed by the root key or any chain owner (`ShortLink.hs:114-116`).
- `ShortLinkCreds.linkRootSigKey` exists for a writer that is not the root (`Agent/Protocol.hs:1602`). `validateOwners` accepts such a writer when its key is in the chain (`:1991-2001`).
- `RKEY` gives a link queue several recipient keys, and any of them authorises every recipient command (`Server.hs:1288-1289`, `:1574`).

The single-owner assumption lives in chat. `groupLinkData` republishes only the creator as owner (`Internal.hs:1670-1683`), only the creator's agent holds the link queue, and `APIMembersRole` refuses the owner role in channels (`Commands.hs:2994-2995`).

The design rests on five decisions:

- **No SMP change.** A promoted owner writes link data with today's `LSET`. The server requires the stored encrypted fixed data to be resent byte-identical (`QueueStore/STM.hs` `addQueueLinkData`), and `LGET` returns it exactly as stored (`Server.hs:1671-1672`). Write access comes from `RKEY`.
- **One key per owner.** An owner's member key is its `OwnerAuth.ownerKey`, the key that signs the link data it writes, and its recipient key on the link queue. The exception is the creator: it keeps its agent's queue key and publishes the public half (§3).
- **Every link write by an owner of a multi-owner channel reads the link first** and changes only what its action changes (§2).
- **A promotion requires the promoter to have verified the new owner's security code.** There is no invitation handshake (§6).
- **The roster stays a single-writer protocol for members and relays**, with a few rules for owners that write concurrently (§4).

## 1. What lives where

| State | Lives on | Written by | How it converges |
|---|---|---|---|
| Link data: profile, relay list, owners chain, departed owners, count | one blob on the link queue's SMP server | any owner | read, change one thing, write; owners and departed owners only grow; the writer re-reads after owner and relay changes |
| Link queue recipient keys | the same queue | any owner | computed from the link data; set with `RKEY` around the link write that changes the owners |
| Roster: member, moderator and admin roles | a signed blob on each relay; signed per-member events | any owner | today's version gate on members and relays; equal-version tie-break and re-send rules on owners |
| Subscriber count | link data | the creator | frozen when the creator leaves |

There is no leader and no consensus (Appendix).

## 2. Link data

**Contents.** The link data holds the profile, the relay list, the owners chain and the count, as today. Chat's `GroupShortLinkData` (`Protocol.hs:1621`) gains two optional fields:

- `revokedOwners :: [MemberId]` lists the owners that have left. It is the tombstone that stops a later write from bringing a departed owner back.
- `creatorQueueKey` is the creator's link-queue recipient public key (§3).

simplexmq's link-data encoding is unchanged.

**A channel is multi-owner** once this device holds an owner row other than its own, current or left. The creator's writes before its first promotion stay as today (`setGroupLinkData`, `setGroupLinkDataAsync`, `Internal.hs:1588-1604`). Every write in a multi-owner channel, by any owner, runs these steps:

1. Read the link data (`LGET`, through the SMP proxy like any link read).
2. Build the write from what was read, applying only this action's change:
   - **profile edit:** the new profile, as a whole value;
   - **owner added:** its entry, appended after the signer's entry, so the chain stays in signing order and `validateLinkOwners` holds as-is;
   - **this owner leaving:** its memberId added to `revokedOwners`, with its own chain entry kept in this write (the agent accepts a non-root writer only while its key is in the chain, `Agent/Protocol.hs:1996-2001`);
   - **relays:** the read list, plus the relays this action adds, minus the relays this action removes, minus relays this device holds as left.
   - The writer then merges what it knows into what was read:
     - owners this device holds as left go into `revokedOwners`;
     - an owner this device holds as current but missing from the chain gets an entry signed by the writer (root key if the writer is the creator, otherwise its member key);
     - an entry in `revokedOwners` is dropped from the chain once no current entry was signed by it, transitively; the writer's own entry in its leave write stays. Entries are about 124 bytes, and the user data is padded to 13,784 bytes (`ShortLink.hs:46`).
   - The count is the read count, unchanged (§5).
3. Write it (`LSET`).
4. If the written chain differs from the read one, set the queue's recipient keys (§3). A promotion sets them before its write instead (§6).
5. After a write that adds an owner, leaves, or adds or removes a relay, read the link data again. If the change is missing, because another owner's write landed in between, repeat once from step 1.

**What this gives up.** Two owners writing within one read-write round trip can lose one write. Steps 2 and 5 limit this to fields nobody re-asserts:

- **Owners and departed owners** come back with the next write of any owner that knows them.
- **A relay change** that the re-read misses has to be repeated by hand. Until then, a relay missing from the published list deactivates itself at its next 15-minute check (`checkRelayServedGroups`, `Commands.hs:6001-6020`). A removed relay that a concurrent write keeps listed is skipped by joiners, because its link no longer accepts them.
- **A profile edit** has to be repeated by hand. Members who applied the lost edit's `XGrpInfo` show it until they next open the channel and read the link data (`updateGroupFromLinkData`, `Internal.hs:1634-1654`).

**Reconcile.** An owner reads the link data when it opens the channel and when its chat starts. Owners run `APIGetUpdatedGroupLinkData` like subscribers: the owner exclusion at `Commands.hs:1968` and in the apps (`ChatView.swift:764-779` and its Kotlin counterpart) is removed. From the read it:

- applies the profile, as subscribers do;
- announces any chain entry, not in `revokedOwners`, for a member it holds neither as owner nor as left (§6);
- connects to listed relays it has no relay member for (§8).

The creator also publishes its count during reconcile (§5), and only the creator makes automatic link writes. A promoted owner writes only from its user's actions (§9).

## 3. Link queue recipient keys

The queue's recipient key set is computed from the link data:

- `creatorQueueKey`, while the creator's memberId is not in `revokedOwners`;
- the `ownerKey` of every other chain entry not in `revokedOwners`.

The creator's queue was created with a key its agent generated. The creator keeps it, so its link connection and subscriptions are unchanged, and publishes the public half as `creatorQueueKey` in the write that adds its first co-owner. A promoted owner's recipient key is its member key, which is already public in the chain. Either way, link holders can see the queue's recipient public keys; they authorise nothing without the private keys.

`RKEY` replaces the whole set, so every `RKEY` sends the full set computed from the link data the writer just read or wrote. The order of steps keeps the set consistent with the chain:

- **Promotion:** `RKEY` adding the new owner's key comes first, then the chain write (§6). If the promoter stops between them, the new key is authorised but unused (the new owner has not received the queue's recipient ID), and the next owner change drops it.
- **Leave:** the `revokedOwners` write comes first, then `RKEY` without the leaver's key (§7). If the leaver stops between them, the next owner change by anyone drops the key.

**What this gives up.** Two owner changes within one round trip can each send a key set computed from data without the other's change. Then a new owner's key can be missing, so its link writes fail with `AUTH`, or a leaver's key can stay authorised. The next owner change by any owner recomputes the set. The re-read in §2 step 5 sees the chain but not the key set.

`recipientKeys` is `NonEmpty` (the server rejects an empty set), and "at least one current owner" is a client-side precondition of leaving (§7).

## 4. Roster

The roster is the owner-signed set of member, moderator and admin roles. Owners are in the link chain, not in the roster. It has two forms:

- signed per-member events (`x.grp.mem.role`, `x.grp.mem.del`) that carry a roster version;
- a versioned blob, which relays store and serve to joiners and to members that see a version gap (`serveRoster`, `Internal.hs:1295-1315`).

It is a single-writer protocol. Members and relays apply an event at a version at or above their gate and advance the gate (`applyAtRosterVersion`, `Subscriber.hs:3404-3446`). That gate is also what stops a relay from replaying old signed role events. A completed blob replaces the roster and reverts privileged members absent from it (`Subscriber.hs:3572-3578`, `processRosterEntries` `:3610-3619`). An owner makes each change at its gate plus one (`broadcastRoster`, `Internal.hs:2642-2645`).

Members and relays keep this protocol unchanged. With several owners, two owners can make a change at the same version. Members and relays apply both events, because the gate accepts an equal version, but a relay keeps whichever blob arrives last. Six rules handle concurrent owners:

1. **Equal-version tie-break on relays.** At the stored blob's version (`stored_roster_version`), a relay replaces its stored blob only if the incoming blob's author memberId is not lower than the stored author's, or if the stored author (`roster_sending_owner_gm_id`) is no longer a current owner. Today the later arrival wins (`Subscriber.hs:3572-3578`). With the tie-break, every relay serves the same blob whatever the arrival order.
2. **Concurrency re-send.** An owner that applies another owner's role or removal event at a version equal to its own gate re-sends its current blob to relays at that version. Members already hold both changes. On relays, the blob that won rule 1 may have reverted one of them. The re-sent blobs are built after the owner applied both changes, so whichever wins rule 1 restores it. The re-send adds no version, so members see no gap.
3. **Departure re-send.** When an owner applies another owner's leave, it re-sends its current blob to relays at its gate. Joiners verify a served blob against its author's key (`Subscriber.hs:3506`, `:4019-4020`). They have no key for a departed author, because joiners skip departed owners (§7). Rule 1 lets the current owner's blob replace the departed author's at an equal version.
4. **Owners are outside the roster.** Applying a roster skips members who are current owners. Otherwise a blob made while the new owner was an admin would set it back to admin. An `x.grp.mem.role` whose target is a current owner or a removed member is ignored, and `APIMembersRole` refuses an owner target.
5. **An owner's own change advances its complete frontier.** Today only received events advance it (`Subscriber.hs:3413-3421`), so every own change leaves an owner a false gap. On another owner's next event it would then fetch and apply a served blob, possibly one without its own latest change (`Subscriber.hs:3438-3446`).
6. **Owners request the roster at chat start.** Each owner sends each relay `XGrpRosterRequest` with its complete frontier, as members do on a gap. An owner back after message expiry (21 days by default) then holds the current gate before it changes roles.

**What this gives up.**

- **A dropped role change.** A change can be dropped on some devices and relays when its version is below their gate, and a relay that drops an event does not forward it (`Subscriber.hs:3423-3425`, `:1133-1144`). This happens:
  - when two owners change roles within seconds of each other and one of them makes more than one change in that time, or three owners do. A device that receives the later of one owner's changes first drops the other owner's change below it, and rule 2 cannot restore it, because the re-sent blob is below the relays' gate;
  - when an owner changes a role right after a long absence, before its request (rule 6) is answered.

  The author still shows the change, and nobody is told it was dropped. Repeating it fixes it.
- **The last changes before a leave.** A role change made seconds before an owner leaves can reach a device after the leave, and devices ignore content from a departed author (`Subscriber.hs:4019-4020`). A relay can also forward an owner's events out of order (`Store/Delivery.hs:178-202`, `Delivery.hs:115-120`; this exists in single-owner channels too, §11). The remaining owners' blob re-send (rule 3) carries the change to relays only if they applied it.

None of this lets a relay do more than it can do today: delay or reorder one owner's events.

## 5. Subscriber count

The count is cosmetic. Today the owner branch of `updatePublicGroupData` (`Internal.hs:1620-1627`) recounts and republishes on every join, leave and removal. In a multi-owner channel:

- That branch runs only on the creator. A promoted owner has no group link row, so the branch would fail there, and its tally misses the subscribers who joined before its promotion.
- The creator publishes its tally only during its reconcile (§2), through the read-merge write, instead of on every event.
- Every other write carries the count it read.
- When the creator leaves, the count stays at its last published value (§11).

## 6. Owner promotion

**Trigger.** `APIMembersRole [M] GROwner` on a channel, by any owner. It is refused unless:

- M is the only target and a current member with a key, not an owner, and not in `revokedOwners`. A former owner cannot be promoted again with the same identity.
- **The promoter has verified M's security code.** `member_security_code` (`setGroupMemberVerified`, `Store/Groups.hs:1754-1760`) must equal the code computed from both keys now (`getChannelMemberCode`, `Commands.hs:3811-3815`).
  - The promoter knows M's key from relay-asserted intros. A relay that substituted it would become an owner, and owners cannot be removed (§11). The out-of-band comparison detects the substitution.
  - Checking that M confirms the key over a relay would not: a substituting relay would confirm with its own key.
- Every current relay and M support `multiOwnerVersion` (the next `VersionChat`).

**Steps** (synchronous, under the group lock):

1. Read the link data.
2. `RKEY`: the set computed from what was read, plus M's member key. The creator's first promotion adds `creatorQueueKey`.
3. Write the link data with M's `OwnerAuth` appended. The creator signs it with the root key (as it signs itself, `Commands.hs:2757`); any other owner signs it with its member key, appended after its own entry. If the write fails (for example the link data would not fit), send `RKEY` with the read set and stop.
4. Re-read the link data. If M's entry is missing, repeat once from step 1.
5. Set M's role to owner locally, and send `x.grp.owner.add {memberId, ownerAuth, profile, linkRcvId}` to the relays.

**The commit, `x.grp.owner.add`.** Every relay and member accepts it only when all of these hold:

- the sender is a current owner (the event requires a signature);
- `ownerAuth.ownerId` is `memberId`;
- `ownerAuth` is signed by the root key stored at join, or by an owner key this device holds, current or left (a delegated entry stays valid after its signer leaves, §2);
- this device does not hold `memberId` as a member that left or was removed.

It then sets the member's role to owner and its key to `ownerAuth.ownerKey`, replacing a different relay-asserted key with a warning. If the member is unknown, it is created with the profile from the event. Relays introduce owners only to later joiners (`introduceInChannel`, `Internal.hs:1320-1332`), so an earlier subscriber may not know M.

Relays forward the event to members. It is a new event, not `x.grp.mem.role … GROwner`, for two reasons:

- older clients ignore it and keep rejecting M's admin and signed messages, which fails closed;
- `x.grp.mem.role` would raise whatever key a relay asserted for M in an intro (`Subscriber.hs:3465-3468`).

**M's own device.** M applies a commit about itself only if `ownerKey` is its own member key. Otherwise it shows an alert and stays a subscriber: the chain names a key that is not its own.

M then stores `linkRcvId` and becomes an owner. It already has connections to the relays it joined as a subscriber, and those relays now treat it as owner. It holds every credential for link writes:

- the server and `LinkKey` from the channel link;
- `linkRcvId` from the commit;
- the root public key stored at join;
- its member private key.

It sends none of them to the link server until its user acts (§9).

**`linkRcvId` reaches every member with the commit.** It authorises nothing without a key in the recipient set, and relays see every message anyway.

**Healing.**

- An owner whose reconcile finds a valid chain entry, not in `revokedOwners`, for a member it holds neither as owner nor as left sends `x.grp.owner.add` for that entry, including `linkRcvId`. This covers a promoter that stopped after its write, and a commit lost or expired on the way. It needs no link-server command.
- Members apply the same chain additions when they open the channel and read the link data. They never do this for a member they hold as left.
- A chain entry lost to a concurrent write comes back with any owner's next write (§2).

**What the new owner sees.** It knows the subscribers it knew before, those who join after its promotion (relays announce joiners to every owner, `introduceInChannel`, `Internal.hs:1320-1326`), and those who post. It cannot see, verify or promote a silent subscriber who joined earlier (§11).

## 7. Leaving, and destroying the channel

**Leave.** Any owner can leave while the link data it reads shows another current owner, that is another chain entry not in `revokedOwners`. The core refuses the last owner. Steps (synchronous):

1. Read the link data and check that another current owner exists.
2. Write the link data with its own memberId added to `revokedOwners`, keeping its own entry (§2).
3. Re-read, and repeat once if the entry is missing.
4. `RKEY` with the set computed from what was written, which no longer includes its key.
5. Send `x.grp.leave` to the relays and delete its connections, as today.
6. The creator deletes its link connection locally, without `DEL`, and erases its root private key. A promoted owner clears its `linkRcvId`.

**Why no `DEL`.** Today `APILeaveGroup` deletes the link connection with `DEL` on the shared queue (`deleteGroupLinkIfExists`, `Commands.hs:3246`, `Internal.hs:1739-1748`). After step 4 the creator's key is normally no longer authorised, but a concurrent `RKEY` by another owner can authorise it again (§3), so the deletion is local. Erasing the root key protects only against a later compromise of the departed device. The root key cannot be revoked: it is the trust anchor in the signed fixed data.

**Members and relays** handle an owner's `x.grp.leave` as today. They mark the owner left, drop its connection, and from then on ignore its content (`xGrpLeave`, `Subscriber.hs:3815-3826`; `Subscriber.hs:4019-4020`). Left rows are never cleaned up: only removed members get `removed_at` (`Internal.hs:2192-2203`).

**Joiners** take owners from the chain as today (`Commands.hs:2324-2332`). They skip memberIds in `revokedOwners`, creating them as members that left, so a relay replaying an old `x.grp.owner.add` for a departed owner, or forwarding its content, is ignored.

**Remaining owners** re-send their roster blob when they apply the leave (§4 rule 3), and drop the leaver's chain entry in a later write once no current entry was signed by it (§2).

**Destroying the channel** stays any current owner's action through `APIDeleteChat`, which sends `x.grp.del` (`Commands.hs:1384-1400`). The creator deletes the link queue through its link connection, as today. A promoted owner deletes it with `DEL` using its link credentials, and only when it sends `x.grp.del`. `deleteGroupLinkIfExists` runs whether or not `x.grp.del` was sent, so it never sends this `DEL`.

**Group-link commands.** `APICreateGroupLink`, `APIAddGroupShortLink` and `APIDeleteGroupLink` are refused for channels (`Commands.hs:3349-3384`):

- a channel's link is created with the channel;
- the apps would otherwise create a direct group link for a promoted owner that opens the link screen (`GroupLinkView.swift:164-166`);
- `APIDeleteGroupLink` sends `DEL` on the shared queue.

## 8. Relays

- **Invitation.** A relay invited by any chain owner reads the link data as today and checks the inviting owner's key against the chain (`getLinkDataCreateRelayLink`, `Subscriber.hs:4586-4599`). `updateRelayGroupKeys` creates an owner row for each chain owner it has no row for; today it throws (`Store/Groups.hs:2366-2373`). For memberIds in `revokedOwners` it creates rows for members that left.
- **Commit.** A relay applies `x.grp.owner.add` (§6) and forwards it, which upgrades M's existing subscriber row. It is added to the relay forwarding set (`isForwardedGroupMsg`, `Protocol.hs:528`) and to forwarded-event handling (`processForwardedMsg`, `Subscriber.hs:4041-4064`).
- **An owner joins a relay another owner added.**
  - **How the owner joins.** It joins through the relay's link, the path subscribers use. It connects this way when it receives `XGrpRelayNew` (`xGrpRelayNew`, `Subscriber.hs:3881-3886`), and in reconcile for any listed relay it has no relay member for.
  - **Relay side.** The relay accepts a join onto an existing owner row when the join is signed with that row's key, with the same `viaRelay` replay check as for roster roles (`memberJoinRequestViaRelay`, `Subscriber.hs:1696-1714`). It refuses any join onto a row of a member that left or was removed; today the roster branch checks the key but not the status.
  - **Owner side.** Completing the join updates only the relay member. `updatePreparedUserAndHostMembers'` would replace the owner's group profile with the relay's copy and, for the creator (`GSMemCreator`), its membership role and status (`Store/Groups.hs:821-844`). The `CON` handler for a host member would set the membership connected and create the joined-channel items (`Subscriber.hs:892-922`).
- **History.** A relay serves an as-channel history item whose signed author is no longer a current owner as an unsigned channel item (`FwdChannel`). Joiners have no key or owner role for a departed author and would drop it. Today a signed as-channel item goes out under its author (`itemForwardMsgs`, `Internal.hs:1540-1548`).

## 9. Security and privacy

- **Owner keys come only from signed chain entries and owner-signed commits.** Relays still cannot create owners. The verified code (§6) means a relay that substituted a member's key cannot become an owner through a promotion.
- **Any owner can destroy the channel.** Any recipient key authorises `DEL` and `LDEL`. Any owner can also replace the key set with `RKEY`, locking the others out of the link. Each owner is trusted as today's single owner is; this is inherent to equal owners. Owners cannot remove one another, so a lost or compromised owner device stays an owner (§11).
- **The link server sees a promoted owner's network address** when it writes. Recipient commands are not proxied (`Server.hs:2205-2211`), so its device connects directly to the server the creator chose. That device sends link-server commands only when its user acts: a profile edit, a relay change, a promotion, a leave, or destroying the channel. The app says so before the first one. These commands use a transport session of their own per link (the default session is shared per user and server, `mkTSession`, `Agent/Client.hs:1442-1443`), so the server cannot tie them to the owner's other queues there. The creator has the same exposure today. Tor or SOCKS avoids it.
- **The promoted member is not asked first,** as with admin promotion today, which also reveals the member to every joiner (`introduceInChannel`, `Internal.hs:1320-1332`).
- **The creator is identifiable among owners.** Its chain entry is first, its link writes are signed with the root key, and the owners it promotes carry root-signed entries. This qualifies `channels-overview.md:159`.
- **Races between owners are not adversarial.** Owner and link-data races (§2, §3) and roster races (§4) need two honest owners acting within seconds. A relay can cause nothing beyond delaying or reordering one owner's events, as today.

## 10. UI/UX

- **Promoter.** The role picker offers Owner in channels. If the member's code is not verified, it routes to the verification screen first. The confirmation states that owners cannot be removed and that any owner can delete the channel.
- **New owner.** It sees a service item saying it became an owner. Before its first link-changing action, the app states once that the device will connect directly to the channel's link server.
- **Link screen.** A promoted owner sees the channel's published link (`publicGroup.groupLink`), as subscribers do. The screen never creates a link for a channel.
- **Leave.** An owner can leave a channel. Leaving as the last owner shows the core's refusal.
- **Relays screen.** For a promoted owner it lists the channel's relay members, not only the relays it added.

## 11. Later

- **Owner removal.**
  - `RKEY` without the target comes first. A remover that gets `AUTH` was itself removed first, so mutual removals serialise at the server.
  - Then a `revokedOwners` write, a re-read, and `x.grp.mem.del`.
  - The removed owner's own device handles it as its removal.
  - Removed owners' rows are never cleaned up.
  - Relays apply `revokedOwners` in their 15-minute link check.
  - A race with a concurrent promotion is closed only by a compare-and-swap on the server (`LSET`/`RKEY` with an expected hash, a new SMP version).
- **Asking the member before promotion**, or promoting over a direct contact, which also carries the key end-to-end.
- **A new owner's member list.** Relays send it their current members on the commit.
- **A relay-sourced subscriber count** that keeps updating after the creator leaves.
- **Multi-writer roster convergence**, so that concurrent or stale role changes by several owners all reach every device. It would include per-member ordering on owners, relays reporting dropped events to their author, and catch-up from relays. It would also carry a departing owner's last changes.
- **Profile edits merged field by field**, and stale profile events rejected by a version in the link data.
- **Relays forwarding in received order, and messages to a backlogged relay queued instead of dropped.** These are single-owner fixes too (`Store/Delivery.hs:178-202`, `Messages/Batch.hs:88-95`, `Internal.hs:2840-2846`).
- **The creator retiring the root key** after its first promotion.

## Appendix — Why no consensus or leader

Link data and recipient keys live on one SMP server, which orders the writes. Read, merge and write, with grow-only owners and departed owners, converges without consensus. A compare-and-swap would only close races between honest owners.

The roster has no single server. A quorum of owners would need several owners online to change a role. Multi-owner exists to survive the absence of owners, so a quorum inverts the feature. A relay quorum would need more relays than channels have, and it would trust relays with ordering, which the current model does not.

A designated leading owner, or a designated relay, would bring back the single point that multi-owner removes. So the roster keeps the single-writer protocol, and owners that write concurrently follow the rules in §4.
