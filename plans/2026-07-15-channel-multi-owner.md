# Channel multi-owner

A channel can have several equal owners, per `docs/protocol/channels-overview.md` §Governance ("v7: any-owner-decides"). Any owner can take any administrative action without coordinating with the others: post, moderate, change the profile, manage relays, update the link data, add owners, destroy the channel. The channel keeps working after any owner, including its creator, leaves. Owners cannot remove one another in this version (§11).

simplexmq already models most of this and leaves it unwired:

- `UserContactData.owners :: [OwnerAuth]` is a signed chain: each entry is signed by the root key or an earlier entry (`validateLinkOwners`, `Agent/Protocol.hs:2003-2017`).
- Readers accept user data signed by the root key or any chain owner (`ShortLink.hs:114-116`).
- `ShortLinkCreds.linkRootSigKey` exists for a writer that is not the root (`Agent/Protocol.hs:1602`), and `validateOwners` accepts such a writer when its key is in the chain (`:1991-2001`).
- `RKEY` gives a link queue several recipient keys, and any of them authorises every recipient command (`Server.hs:1288-1289`, `:1574`).

The single-owner assumption lives in chat. `groupLinkData` republishes only the creator as owner (`Internal.hs:1670-1683`). Only the creator's agent holds the link queue. `APIMembersRole` refuses the owner role in channels (`Commands.hs:2994-2995`).

The design rests on five decisions:

- **No SMP change.** A promoted owner writes link data with today's `LSET`. The server requires the stored encrypted fixed data to be resent byte-identical (`QueueStore/STM.hs` `addQueueLinkData`), and `LGET` returns it exactly as stored (`Server.hs:1671-1672`). Write access comes from `RKEY`.
- **One key per owner.** An owner's member key is its `OwnerAuth.ownerKey`, the key that signs the link data it writes, and its recipient key on the link queue. The exception is the creator: it keeps its agent's queue key and publishes it with its memberId (§3).
- **Every link write by an owner of a multi-owner channel reads the link first** and changes only what its action changes (§2).
- **A promotion requires the promoter to have verified the new owner's security code.** There is no invitation handshake (§6).
- **The roster stays a single-writer protocol for members and relays**, with a few rules for owners that write concurrently (§4).

## 1. What lives where

| State | Lives on | Written by | How it converges |
|---|---|---|---|
| Link data: profile, relay list, owners chain, departed owners, count | one blob on the link queue's SMP server | any owner | read, change one thing, write; owners and departed owners only grow; the writer re-reads after owner and relay changes |
| Link queue recipient keys | the same queue | any owner | computed from the link data; set with `RKEY` around the link write that changes the owners |
| Roster: member, moderator and admin roles | a signed blob on each relay; signed per-member events | any owner | today's version gate on members and relays; tie-break and re-send rules on owners |
| Subscriber count | link data | the creator | frozen when the creator leaves |

There is no leader and no consensus (Appendix).

## 2. Link data

**Contents.** The link data holds the profile, the relay list, the owners chain and the count, as today. Chat's `GroupShortLinkData` (`Protocol.hs:1621`) gains three optional fields:

- `revokedOwners :: [MemberId]` lists the owners that have left. It is the tombstone that stops a later write from bringing a departed owner back, and every party marks the owners it lists as left (§7).
- `creatorQueueKey {memberId, key}` is the creator's memberId and its link-queue recipient public key (§3).
- `linkRcvId` is the link queue's recipient ID, which every owner needs to send recipient commands (§6). It authorises nothing without a key in the recipient set.

The creator writes its own `creatorQueueKey` and `linkRcvId` in every write it makes, so a write that dropped them is repaired by the creator's next one.

simplexmq's link-data encoding is unchanged. Its `UserContactData` parser drops trailing fields it does not know (`Agent/Protocol.hs:2075-2079`), so a future agent-level field needs a version gate, since an older owner's write would drop it.

**A channel is multi-owner** once this device holds an owner row other than its own, current or left. Owner rows are never deleted in channels (§7). The creator's writes before its first promotion stay as today (`setGroupLinkData`, `setGroupLinkDataAsync`, `Internal.hs:1588-1604`).

Every write in a multi-owner channel, by any owner, runs under the group lock, in five steps:

1. Read the link data (`LGET`).
2. Build the write from what was read, applying only this action's change. "What was read" means the read JSON, so fields this version does not know are kept. The action's change is one of:
   - **profile edit:** the new profile, as a whole value;
   - **owner added:** its entry, appended after the signer's entry, so the chain stays in signing order and `validateLinkOwners` holds as-is;
   - **this owner leaving:** its memberId added to `revokedOwners`, with its own chain entry kept in this write (the agent accepts a non-root writer only while its key is in the chain, `Agent/Protocol.hs:1996-2001`);
   - **relays:** the read list, plus the relays this action adds, minus the relays this action removes, minus relays this device holds as left.

   The writer then merges what it knows into what was read:
   - owners this device holds as left go into `revokedOwners`;
   - an owner this device holds as current, missing from the chain and not in `revokedOwners`, gets an entry signed by the writer (root key if the writer is the creator, otherwise its member key);
   - an entry in `revokedOwners` is dropped from the chain once no current entry was signed by it, transitively, except the writer's own entry in its leave write. Entries are about 124 bytes, and the user data is padded to 13,784 bytes (`ShortLink.hs:46`);
   - the creator writes its own `creatorQueueKey` and `linkRcvId`, and every other writer keeps the read ones;
   - the count is the read count, unchanged (§5).
3. Write it (`LSET`).
4. If the written chain differs from the read one, set the queue's recipient keys (§3). A promotion sets them before its write instead (§6).
5. After a write that adds an owner, leaves, or adds or removes a relay, read the link data again. If the change is missing, because another owner's write landed in between, repeat once from step 1. Relays are activated after this read (§8).

**What this gives up.** Two owners writing within one read-write round trip can lose one write. Steps 2 and 5 limit the damage as follows:

- **Owners and departed owners** come back with the next write of any owner that knows them.
- **A relay change** that the re-read misses has to be repeated by hand. Until then, a relay missing from the published list deactivates itself at its next 15-minute check, and deletes its connections to the channel a day later (`checkRelayServedGroups`, `checkRelayInactiveGroups`, `Commands.hs:6001-6026`; `relayInactiveTTL`, `Chat.hs:135`).
- **A removed relay that a concurrent write keeps listed** stays listed for good, because nothing records the removal. Joiners and subscribers fail to connect to it, since its link no longer accepts them, and connect through the other relays.
- **A profile edit** has to be repeated by hand. Members who applied the lost edit's `XGrpInfo` show it until they next open the channel and read the link data (`updateGroupFromLinkData`, `Internal.hs:1634-1654`).
- **A single-owner write the creator queued just before its first promotion** (the agent sends it later, `setGroupLinkDataAsync`) can land after the promotion and drop the new owner's entry, `creatorQueueKey` and `linkRcvId`. The new owner cannot write the link until the creator's next write puts them back; the creator's leave is such a write (§7). Until then, joiners do not take the new owner from the chain, so they treat it as a subscriber, as older members do. The creator keeps its own access: that write does not change the queue's keys.

**Reconcile.** An owner reads the link data when it opens the channel, and when its chat starts in a multi-owner channel. The apps' owner branch of the link-data refresh, and the core's (`Commands.hs:1968`, `ChatView.swift:764-779` and its Kotlin counterpart), are replaced by this reconcile. From the read it:

- applies the profile, as subscribers do;
- applies the owners chain, as every party does (§6, §7);
- connects to listed relays it has no relay member for (§8).

The creator also publishes its count during reconcile (§5). Only the creator makes automatic link writes; a promoted owner writes only from its user's actions (§9).

## 3. Link queue recipient keys

The queue's recipient key set is computed from the link data:

- `creatorQueueKey.key`, while `creatorQueueKey.memberId` is not in `revokedOwners`;
- the `ownerKey` of every other chain entry not in `revokedOwners`.

The creator's queue was created with a key its agent generated. The creator keeps it, so its link connection and subscriptions are unchanged. It publishes the key with its memberId as `creatorQueueKey`: first in the write that adds its first co-owner, then in every write it makes. A promoted owner's recipient key is its member key, which is already public in the chain. Either way, link holders can see the queue's recipient public keys; they authorise nothing without the private keys.

`RKEY` replaces the whole set, so every `RKEY` sends the full set computed from the link data the writer just read or wrote. The order of steps keeps the set consistent with the chain:

- **Promotion:** `RKEY` adding the new owner's key comes first, then the chain write (§6). If the promoter stops between them, the new owner's key is authorised before its entry is published, until the next owner change drops it. Its holder is the member being promoted, whose code the promoter verified.
- **Leave:** the `revokedOwners` write comes first, then `RKEY` without the leaver's key (§7). If the leaver stops between them, its key stays authorised until it repeats the leave or another owner changes the owners.

**What this gives up.** Two owner changes within one round trip can each send a key set computed from data without the other's change:

- A new owner's key can be missing. Its link writes then fail with `AUTH` until the next owner change.
- A departed owner's key can stay authorised until the next owner change, which may never come. The departed owner erased its private key after leaving (§7), so nobody holds it, unless the owner copied it while still an owner. Such an owner was already able to do anything an owner can, including deleting the channel.

The re-read in §2 step 5 sees the chain but not the key set. `recipientKeys` is `NonEmpty` (the server rejects an empty set), and "at least one current owner" is a client-side precondition of leaving (§7).

## 4. Roster

The roster is the owner-signed set of member, moderator and admin roles. Owners are in the link chain, not in the roster. It has two forms:

- signed per-member events (`x.grp.mem.role`, `x.grp.mem.del`) that carry a roster version;
- a versioned blob, which relays store and serve to joiners and to members that see a version gap (`serveRoster`, `Internal.hs:1295-1315`).

It is a single-writer protocol:

- Members and relays apply an event at a version at or above their gate, and advance the gate (`applyAtRosterVersion`, `Subscriber.hs:3404-3446`). That gate is also what stops a relay from replaying old signed role events.
- A completed blob replaces the roster and reverts privileged members absent from it (`Subscriber.hs:3572-3578`, `processRosterEntries` `:3610-3619`).
- An owner makes each change at its gate plus one (`broadcastRoster`, `Internal.hs:2642-2645`).

Members and relays keep this protocol unchanged. With several owners, two owners can make a change at the same version. Members and relays apply both events, because the gate accepts an equal version, but a relay keeps whichever blob arrives last. Seven rules handle owners that write concurrently.

1. **Equal-version tie-break on relays.** At the stored blob's version (`stored_roster_version`), a relay replaces its stored blob only if the incoming blob's author memberId is not lower than the stored author's (`roster_sending_owner_gm_id`). Today the later arrival wins (`Subscriber.hs:3572-3578`). With the tie-break, every relay serves the same blob whatever the arrival order.
2. **Concurrency re-send.** An owner that applies another owner's role or removal event at a version equal to its own gate re-sends its current blob to relays at its gate plus one, as a blob without events. It does this once, after it has processed the whole received message.
   - The blob that won rule 1 may have reverted one of the two changes on relays. The re-sent blob is built after the owner applied both, and its higher version replaces any blob at the old version, whatever its author.
   - Members already hold both changes. Their next event reads as a gap, so they fetch the served blob once (`Subscriber.hs:3438-3446`).
3. **Departure re-send.** When an owner applies another owner's departure, it re-sends its current blob to relays at its gate plus one, as rule 2 does. It does the same when it applies a served blob whose author has left (rule 6). A departure is applied either from the departed owner's `x.grp.leave` or from `revokedOwners` in a link-data read.
   - A departure read from the link data can arrive before the departed owner's last messages, which this device then drops (`Subscriber.hs:4019-4020`), so its gate can stay below the relays'. The owner therefore first asks every current relay for the roster, before its own re-send, so the request carries its frontier from before that re-send. It re-sends again once the served blob is applied (rule 6).
   - Joiners verify a served blob against its author's key and ignore a departed author (`Subscriber.hs:3506`, `:4019-4020`). The re-send's higher version replaces the departed author's blob on every relay.
4. **Owners are outside the roster.**
   - Applying a roster skips members who are current owners, and members held as removed or left. Otherwise a blob made while a new owner was an admin would set it back to admin, and a stale blob would bring back a removed member.
   - An `x.grp.mem.role` or `x.grp.mem.del` whose target is a current owner (this device's own membership included) is ignored, and so is an `x.grp.mem.role` that assigns the owner role. `APIMembersRole` and `APIRemoveMembers` refuse owner targets.
   - A current owner has the owner role and is neither left nor removed.
5. **An owner's own versions advance its complete frontier.** That covers its changes and its blob-only re-sends (rules 2 and 3). Today only received events advance the frontier (`Subscriber.hs:3413-3421`), so every own change would leave an owner a false gap.
6. **An owner sends a blob only when it is in sync.** It sends a blob, for its own change or for rules 2 and 3, only when its complete frontier equals its gate. While it is out of sync:
   - it sends `XGrpRosterRequest` to every current relay, and again with each role change it makes;
   - its role and removal events go out without a blob, so they cannot overwrite relays with a stale view;
   - rules 2 and 3 wait until it is back in sync.

   Every owner, in sync or not, applies a served blob even when the blob's author has left. It verifies the blob with the key it kept for that author, at a version not below its gate. Content from a departed author is otherwise ignored (`Subscriber.hs:4019-4020`). Without this exception, an owner could not catch up after the blob's author left: neither one that is out of sync, nor one that dropped the author's last messages after reading its departure from the link data (rule 3).
7. **An owner back after a long absence requests the roster.** When its chat starts and the newest item it received in the channel is older than message expiry (21 days by default), or there is none, each owner sends each relay `XGrpRosterRequest` with its complete frontier, as members do on a gap. It then holds the current gate before it changes roles.

**What this gives up.**

- **A dropped role change.** A change can be dropped on some devices and relays when its version is below their gate. A relay that drops an event does not forward it (`Subscriber.hs:3423-3425`, `:1133-1144`), and the author still shows the change. This happens:
  - when two owners change roles within seconds of each other and one of them makes more than one change in that time, or when three owners do;
  - when an owner changes a role right after a long absence, before its request (rule 7) is answered.

  When the dropped change is a demotion that some relays had applied, another owner's later blob, made without it, reverts it on those relays. Rule 4 still keeps a removed member removed. A dropped role change is fixed by repeating it. A dropped removal cannot be repeated: the author's member list already shows the member removed.
- **Two owners changing the same member at the same version.** Members keep whichever event they received last, while relays and joiners keep the value in the higher author's blob.
- **An owner that cannot get back in sync.** An owner stays out of sync in two cases:
  - No relay stores a blob at its gate (the last author's blob send failed, and that author has left).
  - It makes a role change before the blob it asked for arrives. The change reserves a version above that blob, so the blob is then refused (`Subscriber.hs:3574-3575`), and relays do not serve it again (`:3685`).

  It stays out of sync until an owner that is in sync sends a blob. A sole remaining owner stays out of sync for good. Its role changes still reach relays and members as events. Joiners, and members that fetch the roster on a gap, get the last blob without them, and a member it demoted shows at its earlier role to them. Relays applied the demotion, so they do not forward that member's actions.
- **The last changes before a leave.** A role change made seconds before an owner leaves can reach a device after the leave, and devices ignore content from a departed author (`Subscriber.hs:4019-4020`). A relay can also forward an owner's events out of order (`Store/Delivery.hs:178-202`, `Delivery.hs:115-120`); this exists in single-owner channels too (§11). A device that reads the link data before it receives a departed owner's last messages drops them as well, posts included. The remaining owners' blob re-send (rule 3) carries the change to relays only if they applied it, or once they apply the served blob. Until then, a remaining owner's own role changes can fall below the relays' gate and be dropped.

These outcomes need two honest owners acting within seconds, or an owner acting right after a long absence. A relay can add nothing beyond delaying or reordering one owner's events, as today.

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
- Every current relay supports `multiOwnerVersion` (the next `VersionChat`).

M's own version is not checked. The promoter cannot know it: it learns a subscriber's version only from that subscriber's join (`Store/Groups.hs:2533-2551`). An older M ignores the commit, and becomes an owner after it upgrades and reads the link data (below).

**Steps** (synchronous, under the group lock):

1. Read the link data. Build the write with M's `OwnerAuth` appended. The creator signs it with the root key (as it signs itself, `Commands.hs:2757`); any other owner signs it with its member key, after its own entry. The creator's first promotion also adds `creatorQueueKey` and `linkRcvId`. Encode, sign and size-check the write now. A promoted owner's write that would not fit fails here, before any key change. The creator's write is size-checked only when it is sent (`setConnShortLink`), so the creator's oversize write fails at step 3, after its `RKEY`.
2. `RKEY`: the set computed from what was read, plus M's member key, plus the creator's own queue key on its first promotion.
3. Write the link data.
4. Re-read the link data. If M's entry is missing, repeat once from step 1. If a step fails after step 2, the command fails, and M's key stays authorised until the next owner change (§3).
5. Set M's role to owner locally. Send `x.grp.owner.add {memberId, ownerAuth, profile}` to the relays. The profile is the redacted one, as member intros send it (`redactedMemberProfile`, `Internal.hs:1361`).

**The commit, `x.grp.owner.add`.** Every relay and member accepts it only when all of these hold:

- the sender is a current owner (owner role, neither left nor removed; the event requires a signature);
- `ownerAuth.ownerId` is `memberId`;
- `ownerAuth` is signed by the channel's root public key, or by an owner key this device holds, current or left (a delegated entry stays valid after its signer leaves, §2);
- this device does not hold `memberId` as a member that left or was removed. Every link-data read turns memberIds in `revokedOwners` into such rows (§7).

It then sets the member's role to owner and its key to `ownerAuth.ownerKey`, replacing a different relay-asserted key with a warning. If the member is unknown, it is created with the profile from the event. Relays introduce owners only to later joiners (`introduceInChannel`, `Internal.hs:1320-1332`), so an earlier subscriber may not know M. Today's admin promotion carries no profile: earlier subscribers see an admin under a name derived from its memberId. A promoted owner's redacted profile reaches every subscriber.

Relays forward the commit to members. It is a new event, not `x.grp.mem.role … GROwner`, for two reasons:

- older clients ignore it and keep rejecting M's admin and signed messages, which fails closed;
- `x.grp.mem.role` would raise whatever key a relay asserted for M in an intro (`Subscriber.hs:3465-3468`).

**Owners from the link data.** Every party applies the owners chain whenever it reads the link data: members when they open the channel, owners in their reconcile, relays at invitation and in their 15-minute check (§8). For each chain entry not in `revokedOwners`, whose member this device holds neither as owner nor as left or removed, the party sets that member as owner with `ownerKey`. The agent has already validated the chain against the root key (`ShortLink.hs:114-116`).

This also covers a promoter that stopped after its write, and a commit that was lost or expired on the way.

**M's own device.** M becomes an owner in either of two cases:

- when it applies the commit about itself;
- when a link-data read shows its memberId in the chain, not in `revokedOwners`.

In both cases `ownerKey` must be its own member key, and its membership must be current. Otherwise it shows an alert and stays a subscriber.

M already has connections to the relays it joined as a subscriber, and those relays now treat it as owner. It holds every credential for link writes, and stores nothing new:

- the server and `LinkKey` from the channel link;
- `linkRcvId`, which each of its link writes takes from the link data it reads first (§2);
- the root public key;
- its member private key.

It sends none of them to the link server until its user acts (§9).

**A removal of M racing M's promotion.** Each device ends with whichever event it applied first. Where the commit came first, the removal targets a current owner and is ignored (§4 rule 4). Where the removal came first, the commit is refused. M ends removed on some devices and an owner on others and in the link.

**What the new owner sees.** It knows:

- the subscribers it knew before;
- those who join after its promotion (relays announce joiners to every owner, `introduceInChannel`, `Internal.hs:1320-1326`);
- those who post.

It cannot see, verify or promote a silent subscriber who joined earlier (§11).

## 7. Leaving, and destroying the channel

**Leave.** Any owner can leave while the link data it reads, merged as every write merges it (§2), shows another current owner, that is another chain entry not in `revokedOwners`. The core refuses the last owner. Steps (synchronous):

1. Read the link data, merge it as every write does (§2), and check that the result shows another current owner. The merge re-adds a current owner missing from the chain.
2. Write the link data with its own memberId added to `revokedOwners`, keeping its own entry (§2).
3. Re-read, and repeat once if the entry is missing.
4. `RKEY` with the set computed from what was written, which no longer includes its key.
5. Send `x.grp.leave` to the relays and delete its connections, as today.
6. Erase its link credentials and keys:
   - the creator deletes its link connection locally, without `DEL`;
   - the creator erases its root private key, keeping the root public key;
   - every leaver erases its member private key. A later load of the group generates an unrelated key, so the erased one is gone.

**Repeating a leave.** A crash or a failed step leaves the leave half done. What happens next depends on where it stopped:
- **Before step 5.** The membership stays current, because applying `revokedOwners` never changes this device's own membership (below). The app keeps offering Leave. A repeated leave that finds its memberId already in `revokedOwners` continues at step 4, and an `AUTH` error at step 4 counts as done. Until then, the device keeps its key, and devices that have not read the link data since still treat it as owner. Deleting the chat in this state is still a current owner's destroy: it sends `x.grp.del` and `DEL` on the link queue, the creator's through its link connection, a promoted owner's with its credentials (below).
- **After step 5.** The chat is left. Deleting it completes step 6's link deletion locally, never with `DEL` (below).

**Why no `DEL`.** Today `APILeaveGroup` deletes the link connection with `DEL` on the shared queue (`deleteGroupLinkIfExists`, `Commands.hs:3246`, `Internal.hs:1739-1748`). After step 4 the creator's key is normally no longer authorised, but a concurrent `RKEY` by another owner can authorise it again (§3), so the deletion is local.

**Why erase the keys.** A key that a race leaves authorised then has no holder. The root key cannot be revoked: it is the trust anchor in the signed fixed data.

**Revocation is durable on every device.** In channels:

- **Owner and privileged rows are never deleted.** Item expiry, Clear chat and removed-member cleanup leave alone every member row with the owner role or a roster role (`getGroupMembersForExpiration`, `Store/Groups.hs:1357-1373`, used at `Commands.hs:1429-1430` and `:6078-6079`; `getRemovedMembersToCleanup`, `Store/Groups.hs:1375-1382`). Today these delete left and removed members that have no chat items.
- **An intro never changes the status of a member held as left or removed.** Today an intro sets any existing channel member current (`xGrpMemIntro`, `Subscriber.hs:3314-3322`, `updatePreparedChannelMember`, `Store/Groups.hs:3608-3623`).
- **Every link-data read applies `revokedOwners`.** Every member row whose memberId is listed is marked left, whatever role this device holds it at, because a device that missed the commit holds the owner at its earlier role. This device's own membership is the exception: a listed membership means a half-done leave. A relay also drops that member's connection, as `xGrpLeave` does. For memberIds it holds no row for, it creates rows of members that left. This happens on members when they open the channel, on owners in reconcile, and on relays at invitation and in their 15-minute check (§6).

A departed owner's later content is then ignored everywhere (`Subscriber.hs:4019-4020`), as are a replay of its old `x.grp.owner.add` and an intro that would revive it. A device that has not read the link data since the leave, and whose relays withheld the leave, keeps treating the departed owner as current until its next read.

**Members and relays** handle an owner's `x.grp.leave` as today: they mark the owner left and drop its connection (`xGrpLeave`, `Subscriber.hs:3815-3826`). **Joiners** take owners from the chain as today (`Commands.hs:2324-2332`), and create memberIds in `revokedOwners` as members that left. **Remaining owners** re-send their roster blob when they apply the leave (§4 rule 3), and drop the leaver's chain entry in a later write once no current entry was signed by it (§2).

**Two last owners leaving at once.** Each can read the other as current and verify its own write before the other's write lands. Both leave, and the channel ends with no owner.

**Destroying the channel** stays any current owner's action through `APIDeleteChat`, which sends `x.grp.del` (`Commands.hs:1384-1400`). The creator deletes the link queue through its link connection, as today. A promoted owner deletes it with `DEL` using its link credentials, and only when it sends `x.grp.del`. `deleteGroupLinkIfExists` runs whether or not `x.grp.del` was sent, so it never sends this `DEL`. A creator whose membership is no longer a current owner, for example after a leave that stopped after step 5, deletes its link connection locally when it deletes the chat, never with `DEL`.

**Group-link commands.** `APICreateGroupLink`, `APIAddGroupShortLink` and `APIDeleteGroupLink` are refused for channels (`Commands.hs:3349-3384`):

- a channel's link is created with the channel;
- the apps would otherwise create a direct group link for a promoted owner that opens the link screen (`GroupLinkView.swift:164-166`);
- `APIDeleteGroupLink` sends `DEL` on the shared queue.

## 8. Relays

- **Invitation.** A relay invited by any chain owner reads the link data as today, and checks the inviting owner's key against the chain, excluding `revokedOwners` (`getLinkDataCreateRelayLink`, `Subscriber.hs:4586-4599`). `updateRelayGroupKeys` creates an owner row for each chain owner it has no row for, as joiners do (`createLinkOwnerMember`); today it throws (`Store/Groups.hs:2366-2373`). It applies `revokedOwners` (§7).
- **15-minute check.** The relay's periodic link check, which already reads the link data (`checkRelayServedGroups`, `Commands.hs:6001-6020`), also applies the owners chain and `revokedOwners` (§6, §7).
- **Commit.** A relay applies `x.grp.owner.add` (§6) and forwards it, which upgrades M's existing subscriber row. It is added to the relay forwarding set (`isForwardedGroupMsg`, `Protocol.hs:528`) and to forwarded-event handling (`processForwardedMsg`, `Subscriber.hs:4041-4064`).
- **Relay removal and a relay's leave.**
  - The owner that removes a relay publishes the removal through §2, subtracting that relay. Today only the creator's `updatePublicGroupData` publishes it, and the removed relay's member row is deleted (`Internal.hs:2195-2196`).
  - A relay that leaves on its own is published by the creator when it processes the leave.
  - Any owner's later write subtracts a relay it holds as left.
- **An owner joins a relay another owner added.**
  - **How the owner joins.** It joins through the relay's link, the path subscribers use. It connects this way when it receives `XGrpRelayNew` (`xGrpRelayNew`, `Subscriber.hs:3881-3886`), and in reconcile for any listed relay it has no relay member for.
  - **Relay side.** The relay accepts a join onto an existing owner row when the join is signed with that row's key, with the same `viaRelay` replay check as for roster roles (`memberJoinRequestViaRelay`, `Subscriber.hs:1696-1714`). It refuses any join onto a row of a member that left or was removed; today the roster branch checks the key but not the status.
  - **Owner side.** Completing the join updates only the relay member. `updatePreparedUserAndHostMembers'` would replace the owner's group profile with the relay's copy and, for the creator (`GSMemCreator`), its membership role and status (`Store/Groups.hs:821-844`). The `CON` handler for a host member would set the membership connected and create the joined-channel items (`Subscriber.hs:892-922`).
  - **History.** The relay sends the joining owner recent history as to any joiner. Posts older than message deduplication (30 days) come back to it as received items.
- **History for joiners.** A relay serves an as-channel history item whose signed author is no longer a current owner as an unsigned channel item (`FwdChannel`). Joiners have no key or owner role for a departed author and would drop it. Today a signed as-channel item goes out under its author (`itemForwardMsgs`, `Internal.hs:1540-1548`).

**What this gives up.**

- **A relay addition can stay unpublished.** This happens in two ways:
  - The background link write after the relay's acknowledgement is not retried if it fails. The relay has already moved to `RSAcknowledgedRoster`, which nothing publishes again (`xGrpRosterAck`, `Subscriber.hs:3658-3672`), so only removing and re-adding the relay recovers it.
  - If another owner's role change advanced the adding owner's roster version before the relay acknowledged, the acknowledgement no longer matches and only warns (`Subscriber.hs:3660-3667`). The relay stays unpublished until the owner next sends a blob (§4).
- **Inviting a relay that already serves the channel.** A promoted owner cannot tell which configured relay already serves the channel, because relay links are per channel. If it invites such a relay, that relay creates a second group for the channel.

## 9. Security and privacy

- **Owner keys come only from signed chain entries and owner-signed commits.** Relays still cannot create owners. The verified code (§6) means a relay that substituted a member's key cannot become an owner through a promotion.
- **Any owner can destroy the channel.** Any recipient key authorises `DEL` and `LDEL`. Any owner can also replace the key set with `RKEY`, locking the others out of the link. Each owner is trusted as today's single owner is; this is inherent to equal owners. Owners cannot remove one another, so a lost or compromised owner device stays an owner (§11).
- **Departed owners keep no power.**
  - Revocation is durable on every device (§7).
  - A departed owner erased its keys (§7).
  - A departed owner that kept a copy of its member key, or the creator a copy of its root key, while still an owner can still act with it:
    - with the link's SMP server's help, it can serve forged link data to readers, including new owner entries that every party then applies (§6);
    - through a colluding relay, it can serve a roster blob it signs with the kept key, at any version not below an owner's gate, which that owner applies and re-sends (§4 rules 3 and 6). A colluding relay alone can replay a blob the departed owner signed before leaving.

    Such an owner was already able to do anything an owner can.
- **The link server sees a promoted owner's network address** when it writes. Recipient commands are not proxied (`Server.hs:2205-2211`), so the owner's device connects directly to the server the creator chose.
  - That device sends link-server commands only when its user acts: a profile edit, a relay change, a promotion, a leave, or destroying the channel. The app says so before the first one.
  - These commands, and the link reads in the same write when they are not proxied, use a transport session of their own per link. The default session is shared per user and server (`mkTSession`, `Agent/Client.hs:1442-1443`), so a shared session would let the server tie the owner's link writes to its other queues there.
  - The creator has the same exposure today. Tor or SOCKS avoids it.
- **The promoted member is not asked first,** as with admin promotion today, which also reveals the member to every joiner (`introduceInChannel`, `Internal.hs:1320-1332`). The commit also gives every existing subscriber its redacted profile (§6).
- **`linkRcvId` is readable by anyone with the link.** It authorises nothing without a key in the recipient set.
- **The creator is identifiable among owners.** Its chain entry is first, its link writes are signed with the root key, `creatorQueueKey` names it, and the owners it promotes carry root-signed entries. This qualifies `channels-overview.md:159`.
- **Races between owners are not adversarial.** The races in §2, §3, §4 and §7 need two honest owners acting within seconds. A relay can add nothing beyond delaying or reordering one owner's events, as today.

## 10. UI/UX

- **Promoter.**
  - The role picker offers Owner in channels. If the member's code is not verified, it routes to the verification screen first.
  - The confirmation states that owners cannot be removed, that any owner can delete the channel, and that a member with an older app becomes an owner after it upgrades.
  - Remove and role change are hidden for owners in channels.
- **New owner.** It sees a service item saying it became an owner. Before its first link-changing action, the app states once that the device will connect directly to the channel's link server.
- **Link screen.** A promoted owner sees the channel's published link (`publicGroup.groupLink`), as subscribers do. The screen never creates a link for a channel.
- **Leave.** An owner can leave a channel. Leaving as the last owner shows the core's refusal.
- **Relays screen.** For a promoted owner it lists the channel's relay members, not only the relays it added.

## 11. Later

- **Owner removal.**
  - `RKEY` without the target comes first. A remover that gets `AUTH` was itself removed first, so mutual removals serialise at the server.
  - Then a `revokedOwners` write, a re-read, and `x.grp.mem.del`.
  - The removed owner's own device handles it as its removal.
  - Relays apply `revokedOwners` in their 15-minute check, as they already do for leaves.
  - A race with a concurrent promotion is closed only by a compare-and-swap on the server: `LSET` and `RKEY` with an expected hash, under a new SMP version. The same change would close every race in §2 and §3.
- **Asking the member before promotion**, or promoting over a direct contact, which also carries the key end-to-end.
- **A new owner's member list.** Relays send it their current members on the commit.
- **A relay-sourced subscriber count** that keeps updating after the creator leaves.
- **Multi-writer roster convergence**, so that concurrent or stale role changes by several owners all reach every device. It would include per-member ordering on owners, relays reporting dropped events to their author, and catch-up from relays. It would also carry a departing owner's last changes.
- **Profile edits merged field by field**, and stale profile events rejected by a version in the link data.
- **A tombstone for removed relays.**
- **Relays forwarding in received order, and messages to a backlogged relay queued instead of dropped.** These are single-owner fixes too (`Store/Delivery.hs:178-202`, `Messages/Batch.hs:88-95`, `Internal.hs:2840-2846`).
- **The creator retiring the root key** after its first promotion.

## Appendix — Why no consensus or leader

Link data and recipient keys live on one SMP server, which orders the writes. Read, merge and write, with grow-only owners and departed owners, converges without consensus. A compare-and-swap would only close races between honest owners.

The roster has no single server. A quorum of owners would need several owners online to change a role, which inverts the feature: multi-owner exists to survive the absence of owners. A relay quorum would need more relays than channels have, and it would trust relays with ordering, which the current model does not.

A designated leading owner, or a designated relay, would bring back the single point that multi-owner removes. So the roster keeps the single-writer protocol, and owners that write concurrently follow the rules in §4.
