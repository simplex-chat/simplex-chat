# Channel multi-owner — implementation plan

Companion to the design overview (`2026-07-15-channel-multi-owner.md`); read that first for *why*. This plan covers *what to build*: agent APIs, data model, types, the protocol event, commands, logic and sequencing. It is a map for implementing agents, not an exhaustive edit list. Trivial wiring is implied: JSON instances, `CMEventTag` cases, view strings.

In short:

- Several equal owners; any owner can promote a member whose security code it has verified, and any owner can leave while another remains.
- No SMP server change. A promoted owner writes the link with today's `LSET`, resending the fixed data it read with `LGET`, and gets write access through today's `RKEY`. `RKEY` was added with short links (simplexmq `b7a95422`, SMP v15), so every server that hosts a channel link accepts it.
- Every link write in a multi-owner channel reads the link data, changes only its own part, writes, and re-reads after owner and relay changes.
- The roster keeps today's single-writer protocol on members and relays.

## Sequencing

Two repositories, in order:

1. **simplexmq client and agent (Part 1).** No server or protocol change, no agent schema change.
2. **simplex-chat (Parts 2–8).**

The relay side ships in the same release as promotion. Promotion is refused while any current relay or the target is below `multiOwnerVersion` (Part 8).

---

## Part 1 — simplexmq (client and agent)

- **SMP client.**
  - Add `setSMPQueueRcvKeys`, which sends `RKEY` signed with a given recipient private key. Today the client has no `RKEY` call; the server handles the command at `Server.hs:1574`.
  - `LSET` (`addSMPQueueLink`, `Client.hs`) and `DEL` (`deleteSMPQueue`) already take an explicit key and queue ID.
- **`LinkOwnerCreds`.** A new agent type holding everything a non-creator owner needs to write a link:
  - `SMPServer` and `LinkKey`, from the channel link;
  - `linkRcvId`, the link queue's recipient ID, delivered in the promotion commit;
  - the root public key;
  - the owner's member private key, which both signs the user data and authorises the queue commands.

  It is assembled by chat from stored values. The agent stores nothing.
- **Read for writing.** `getConnShortLinkForWrite` is `getConnShortLink'` (`Agent.hs:1321-1350`) that also returns the encrypted fixed data from the `LGET` response (`getQueueLink`, `Agent/Client.hs:2015-2020`, proxied as any link read).
  - `LSET` requires the fixed data byte-identical to the stored copy (`QueueStore/STM.hs` `addQueueLinkData`; Postgres the same).
  - `LGET` returns it as stored (`Server.hs:1671-1672`).
  - The fixed data never changes, so one read serves every later write.
- **Write with credentials.** `setLinkDataWithCreds creds encFixedData userContactData`:
  1. Builds `ShortLinkCreds {shortLinkId, shortLinkKey, linkPrivSigKey = owner key, linkRootSigKey = Just root, linkEncFixedData}` in memory.
  2. Runs `validateOwners`, which requires the owner key in the chain (`Agent/Protocol.hs:1991-2001`).
  3. Signs and encrypts the user data as `prepareContactLinkData` does (`Agent.hs:1239-1272`).
  4. Sends `LSET` on `linkRcvId`, signed with the owner key.

  `setLinkRcvKeysWithCreds` (`RKEY`) and `deleteLinkQueueWithCreds` (`DEL`) take the same credentials. All three are synchronous. They run on a transport session of their own per link: `TSMEntity` with the link's recipient ID, where the default session is shared per user and server (`mkTSession`, `Agent/Client.hs:1442-1443`).
- **Creator's own queue.**
  - `getConnLinkQueue connId` returns the link queue's recipient ID and the public half of its `rcvPrivateKey`. It is a local read.
  - `setConnLinkRcvKeys connId keys` sends `RKEY` on the creator's own link queue, signed with its stored key.
  - The creator's link-data writes keep using `setConnShortLink` (`Agent.hs:1239-1250`), which already resends the stored encrypted fixed data.
- **Local delete.** `deleteConnectionLocal connId` deletes a connection and its queue records without any server command. Every agent deletion path today sends `DEL` (`deleteConnectionsAsync_`, `Agent.hs:1022-1030`).
- **`Crypto/ShortLink.hs`.** Extract `mkOwnerAuth :: OwnerId -> PublicKeyEd25519 -> PrivateKeyEd25519 -> OwnerAuth`, define `newOwnerAuth` with it, and export it. Chat signs another member's existing key with it.
- **Tests.**
  - Link data written with a non-root owner key is accepted by a reader.
  - `RKEY` with credentials lets the added key write and stops the removed key.
  - `DEL` with credentials deletes the queue.
  - `getConnShortLinkForWrite` followed by `setLinkDataWithCreds` keeps the fixed data.
  - `deleteConnectionLocal` sends nothing to the server.

---

## Part 2 — simplex-chat: data model

- **Migration** `M20260721_channel_multi_owner` (SQLite and Postgres, registered in `Migrations.hs` and the `.cabal`): `groups.link_rcv_id BLOB`. It is set on a promoted owner from the commit and cleared when it leaves. Together with the server and `LinkKey` from the channel link, the stored `root_pub_key` and the member private key, it makes up `LinkOwnerCreds`.
- **No other table changes.**
  - Owners are member rows with role owner, and their keys are in `member_pub_key`.
  - Owners that left are rows with status `GSMemLeft`, which cleanup never deletes: only `removed_at` rows are cleaned up (`Internal.hs:2192-2203`, `getRemovedMembersToCleanup`, `Store/Groups.hs:1375-1382`).
- **Link data.** Chat's `GroupShortLinkData` (`Protocol.hs:1621`) gains two optional fields. simplexmq's `UserContactData` encoding is unchanged.
  - `revokedOwners :: Maybe [MemberId]` lists the owners that left.
  - `creatorQueueKey :: Maybe PublicKeyEd25519` is the creator's link-queue recipient public key.
- **Multi-owner channel.** A query that is true when this device holds an owner row other than its own membership, current or left. It selects the read-merge write path (Part 6.1).

---

## Part 3 — types

- `LinkOwnerCreds` (simplexmq, Part 1).
- **`GroupLinkWriter = GLWCreator Connection | GLWOwner LinkOwnerCreds`.** It is resolved once per link write from the keys:
  - the root private key (`GRKPrivate`) selects the creator's own link connection;
  - otherwise a stored `link_rcv_id` selects the credentials;
  - neither means the device is not a link writer.

  It is never resolved from whether a group link row exists.
- `CRGroupRelaysAdded` and `CEvtGroupLinkDataUpdated` (`Controller.hs:862`, `:1035`) carry `Maybe GroupLink`. It is absent for a promoted owner, which has no group link row.
- **`GroupOwnerAdd {memberId, ownerAuth, profile, linkRcvId}`** is the payload of `x.grp.owner.add`. `OwnerAuth` is reused from simplexmq.

---

## Part 4 — protocol

- **`x.grp.owner.add`** carries `GroupOwnerAdd` and is in `requiresSignature`. It is sent owner → relays. Relays apply it and forward it to every member:
  - add it to `isForwardedGroupMsg` (`Protocol.hs:528`);
  - add it to the relay's dispatch (`processEvent`);
  - add it to `processForwardedMsg` under `withAuthor` (`Subscriber.hs:4041-4064`), whose catch-all rejects unknown events.

  Older clients do not parse it and ignore it.
- **`multiOwnerVersion = VersionChat 23`.** `currentChatVersion` goes from 22 to 23 (`Protocol.hs:96-97`).
- **No other wire change.** Leaving uses `x.grp.leave`, and roster events and blobs are unchanged.

---

## Part 5 — commands

- **`APIMembersRole [M] GROwner` on a channel** starts the promotion (Part 6.3); the ban at `Commands.hs:2994-2995` is replaced by it. Refusals:
  - more than one target;
  - a target that is not a current member with a key, is already an owner, or is in `revokedOwners`;
  - the promoter has not verified the target's security code: `memberVerifiedCode` must equal `getChannelMemberCode` now (`Commands.hs:3811-3815`, stored by `setGroupMemberVerified`, `Store/Groups.hs:1754-1760`);
  - any current relay, or the target, below `multiOwnerVersion`.
- **`APIMembersRole` with a current owner as a target, to any role, is refused in channels.** Owners are managed only through the chain.
- **`APIRemoveMembers` of an owner is refused in channels.**
- **`APILeaveGroup` by an owner of a multi-owner channel** runs the leave (Part 6.6) instead of `deleteGroupLinkIfExists` (`Commands.hs:3246`). It is refused when no other current owner remains. A sole owner keeps today's behaviour.
- **`APIDeleteChat` by a promoted owner**, in the branch that sends `x.grp.del` (`doSendDel`, `Commands.hs:1394-1400`), also deletes the link queue with `deleteLinkQueueWithCreds`. `deleteGroupLinkIfExists` at `:1400` runs regardless of `doSendDel`, so this `DEL` is not added there.
- **`APICreateGroupLink`, `APIAddGroupShortLink` and `APIDeleteGroupLink` are refused for channels** (`Commands.hs:3349-3384`).
- **`APIGetUpdatedGroupLinkData`** runs for owners too: remove the owner exclusion at `Commands.hs:1968`. For an owner, the subscriber relay sync is replaced by the owner reconcile (Part 6.2).
- **`APIAddGroupRelays`.** A promoted owner takes the channel link from `publicGroup.groupLink`; the creator takes it from its group link row (`Commands.hs:2790-2793`). Publication goes through Part 6.1.
- **`APIShareChatMsgContent`** signs with the member key of any current owner, not only under `GRKPrivate` (`Commands.hs:1238-1240`).

---

## Part 6 — key logic changes (anchors)

### 6.1 Link writes in a multi-owner channel

One function, `writeChannelLink`, takes the change: profile, add owner, leave, relays added or removed, or count.

1. **Read.**
   - The creator reads with `getShortLinkConnReq'` (`Internal.hs:1690-1693`).
   - A promoted owner reads with `getConnShortLinkForWrite`, keeping the encrypted fixed data.
2. **Build the write from what was read, applying only this change (overview §2):**
   - the profile as a whole value;
   - a new owner's entry appended after its signer's entry;
   - the writer's own memberId added to `revokedOwners`, keeping its own entry;
   - relays: the read list, plus the relays this call adds, minus the relays this call removes, minus relays whose member row is left. A relay removal deletes its member row (`deleteOrUpdateMemberRecordIO`, `Internal.hs:2195-2196`), so the removed relays are passed in, not read back.
   - Then merge:
     - owners this device holds as left go into `revokedOwners`;
     - an owner held as current but missing from the chain gets an entry signed by the writer;
     - an entry in `revokedOwners` is dropped once no current entry was signed by it, transitively; the writer's own entry in its leave write stays. Keep the chain in signing order, never a memberId sort: `validateLinkOwners` needs each entry signed by root or an earlier entry (`Agent/Protocol.hs:2003-2017`).
   - Carry the read `publicGroupData` and `creatorQueueKey`.
3. **Write** through the resolved `GroupLinkWriter`: the creator uses `setConnShortLink`, a promoted owner uses `setLinkDataWithCreds`.
4. **Keys.** If the chain changed (and the change is not a promotion, which sets the keys first, Part 6.3), send `RKEY` with the set computed from what was written (overview §3): `creatorQueueKey` unless the creator is in `revokedOwners`, plus the `ownerKey` of every other entry not in `revokedOwners`.
5. **Verify.** After an owner, leave or relay change, read again; if the change is missing, repeat once from step 1.

**Relay activation.** The body of the `LINK` handler (`Subscriber.hs:1431-1481`) becomes a function of the written relay list and the writer: set relays to `RSActive`, send `XGrpRelayNew` to the other relays, emit `CEvtGroupLinkDataUpdated`. It is called after step 3, because these writes are synchronous and produce no `LINK` event. Today the handler re-reads the group link row (`:1439`), which a promoted owner does not have.

**Callers in a multi-owner channel.**

- the profile edit, which today calls `setGroupLinkData'` (`Commands.hs:4190`, `Internal.hs:1581-1586`);
- the relay acknowledgement (`xGrpRosterAck`, `Subscriber.hs:3663-3665`) and the relay connect path for pre-roster relays (`Subscriber.hs:946-947`), which today use `setGroupLinkDataAsync`;
- relay removal, through `updatePublicGroupData` (Part 6.10);
- promotion, leave, and the creator's count publish.

Asynchronous callers run the synchronous write in a thread. A single-owner channel keeps today's functions unchanged.

### 6.2 Owner reconcile

It runs in `APIGetUpdatedGroupLinkData` for owners and once after chat start for each channel the user owns. From the read link data:

- **Profile.** Apply it as subscribers do (`updateGroupFromLinkData`, `Internal.hs:1634-1654`).
- **Unknown owners.** For each chain entry not in `revokedOwners` whose member this device holds neither as owner nor as left, send `x.grp.owner.add` for it. Take the entry from the chain, the profile as held (placeholder if unknown), and the stored `link_rcv_id`, or for the creator `getConnLinkQueue`. This needs no link-server command.
- **Relays.** Join each listed relay this owner has no relay member for, through `connectToRelayAsync` (`Internal.hs:1606-1616`). Unlike `syncSubscriberRelays` (`Commands.hs:3992-4011`), this deletes nothing, so a relay this owner is still adding is kept.
- **Count.** The creator publishes its count if it differs (Part 6.10).
- **Roster.** Send each relay `XGrpRosterRequest` with this owner's complete frontier, at chat start only (Part 6.9 rule 6).

### 6.3 Promotion

Under the group lock, synchronously:

1. Read the link data.
2. `RKEY`: the read set plus M's member key.
   - The creator uses `setConnLinkRcvKeys`. On its first promotion the read data has no `creatorQueueKey`, so the set includes its own queue key from `getConnLinkQueue`, which step 3 publishes as `creatorQueueKey`.
   - Any other owner uses `setLinkRcvKeysWithCreds`.
3. Write the link data with M's entry. The creator signs it with the root key, as `Commands.hs:2757` does for itself. Any other owner signs it with its member key. Both use `mkOwnerAuth` over M's stored key.
   - If the write fails, for example because the link data would not fit in 13,784 bytes (`ShortLink.hs:46`), send `RKEY` with the read set and fail the command.
4. Verify, and repeat once from step 1 if M's entry is missing.
5. Set M's role to owner locally. Send `x.grp.owner.add {memberId, ownerAuth, profile, linkRcvId}` to the current relays. `linkRcvId` is the stored `link_rcv_id`, or for the creator `getConnLinkQueue`.

### 6.4 Applying `x.grp.owner.add`

On relays (`processEvent`) and on members (`processForwardedMsg`). Accept it only when all of these hold:

- the sender is a current owner (signed; the event is in `requiresSignature`);
- `ownerAuth.ownerId == memberId`;
- `ownerAuth` verifies against the stored `root_pub_key` or an owner key this device holds, current or left (a delegated entry stays valid after its signer leaves);
- this device holds no row for `memberId` that left or was removed.

Then:

- Set role owner and `member_pub_key = ownerAuth.ownerKey`, logging a warning when it replaces a different key.
- If the member is unknown, create it with `profile`. If its profile is a placeholder, update it.
- A relay returns the group delivery scope so the event is forwarded.

**On M's own device.** If `ownerAuth.ownerKey` is its own member key:

- set the membership role to owner;
- store `linkRcvId` in `link_rcv_id`;
- create the service item.

Otherwise ignore the commit and show an alert. Either way, M sends nothing to the link server.

### 6.5 Owners from link data on members

`updateGroupFromLinkData` (`Internal.hs:1634-1654`; callers `Commands.hs:1966`, `:4624`, `:4651`) also receives the link's owners and `revokedOwners`. For each entry not in `revokedOwners` whose member this device holds neither as owner nor as left or removed, it creates or upgrades that member as owner with `ownerKey`. The agent has already validated the chain against the root key (`decryptLinkData`, `ShortLink.hs:114-116`).

At join, `APIConnectPreparedGroup` (`Commands.hs:2324-2332`) creates owners only for entries not in `revokedOwners`, and creates rows of members that left for memberIds in `revokedOwners`.

### 6.6 Leave

Under the group lock, synchronously:

1. Read the link data. Refuse unless it shows another current owner: a chain entry other than its own, not in `revokedOwners`.
2. Write with its own memberId in `revokedOwners`, keeping its own entry.
3. Verify, and repeat once if needed.
4. `RKEY` with the set computed from what was written, which no longer includes its own key.
5. Send `x.grp.leave` and delete member connections, as `APILeaveGroup` does today (`Commands.hs:3233-3253`).
6. Clean up its link access:
   - the creator deletes its link connection with `deleteConnectionLocal`, deletes its group link row, and sets `root_priv_key` to NULL;
   - a promoted owner clears `link_rcv_id`.

**On receivers.** `xGrpLeave` (`Subscriber.hs:3815-3826`) is unchanged for members and relays. A remaining owner also re-sends its roster blob (Part 6.9 rule 3).

### 6.7 Relays

- **Invitation.** `updateRelayGroupKeys` (`Store/Groups.hs:2353-2373`, called from `getLinkDataCreateRelayLink`, `Subscriber.hs:4586-4612`) creates an owner row with `ownerKey` for every chain owner it has no row for; today it throws at `:2366-2368`. It creates rows of members that left for `revokedOwners`, decoded from the link data it already reads. The inviting-owner check (`Subscriber.hs:4598-4599`) also refuses an owner in `revokedOwners`.
- **Owner join.** In `memberJoinRequestViaRelay` (`Subscriber.hs:1690-1720`), an existing owner row is accepted like a roster-role row:
  - the join is signed with that row's key;
  - it passes the `viaRelay` check (`:1711-1714`);
  - `acceptJoin` runs with the existing row and role owner.

  A join onto a row whose member left or was removed is refused in both branches. Today the roster branch checks the key but not the status (`:1702-1705`).
- **History.** In `itemForwardMsgs` (`Internal.hs:1449-1460`, `:1540-1548`), a signed as-channel item whose author is no longer a current owner is sent as an unsigned channel item (`FwdChannel`, content re-encoded as for unsigned items).

### 6.8 Owner joining a relay

An owner joins a relay through `connectToRelayAsync`: from `xGrpRelayNew` (`Subscriber.hs:3881-3886`) and from the reconcile (Part 6.2). When the join completes, only the relay member is updated:

- `updatePreparedUserAndHostMembers'` (`Store/Groups.hs:821-844`) skips the membership update and the group profile replacement when the membership role is owner;
- the `CON` branch for a host member (`Subscriber.hs:892-922`) skips the membership status update and the joined-channel items and event.

### 6.9 Roster rules

Members and relays keep `applyAtRosterVersion` and the completion gate. The rules from overview §4:

1. **Tie-break.** In `rosterCompletion` (`Subscriber.hs:3572-3578`), when `pendingVer` equals `stored_roster_version`, keep the stored blob unless one of these holds:
   - the incoming author's memberId is at least the stored author's (from `roster_sending_owner_gm_id`, `getStoredGroupRoster`, `Store/Groups.hs:1615-1630`);
   - the stored author is no longer a current owner.

   Below or above the stored version, today's gate decides.
2. **Concurrency re-send.** In `applyAtRosterVersion` (`Subscriber.hs:3404-3446`), when this device is an owner and applies another owner's event whose version equals its gate, re-send its current roster to every current relay at that version, at most once per received message. Use `sendRoster` (`Internal.hs:2665-2670`) with `buildGroupRoster` over the current roster members.
3. **Departure re-send.** In `xGrpLeave`, when the leaver was an owner and this device is an owner, re-send the same way at its gate.
4. **Owners are outside the roster.**
   - `processRosterEntries` (`Subscriber.hs:3610-3630`) skips entries for current owners.
   - `xGrpMemRole` (`Subscriber.hs:3448-3500`) ignores an event whose target is a current owner or a member that left or was removed.
5. **Own changes advance the frontier.** `broadcastRoster` (`Internal.hs:2642-2645`) sets the complete frontier to the reserved version when it is the next one (`setCompleteRosterVersion`, as `Subscriber.hs:3419-3421` does on receipt).
6. **Chat-start request.** An owner sends `XGrpRosterRequest` with its complete frontier to each relay; relays already answer members this way (`xGrpRosterRequest`, `Subscriber.hs:3679-3685`).

### 6.10 Count

In `updatePublicGroupData` (`Internal.hs:1620-1630`):

- The owner branch runs only for the creator, recognised by `GRKPrivate`. For a promoted owner it does nothing; today it would throw on the missing group link row.
- In a multi-owner channel the creator's branch recounts but does not publish on joins, leaves and removals. It does publish when the change is a relay removal: Part 6.1 with the removed relays.
- The creator's reconcile publishes the count when it differs from the published one.
- Every other write carries the read count.

---

## Part 7 — apps (iOS and Kotlin, mirrored)

- **Role picker.** Add Owner for channels (`canChangeRoleTo`, `ChatTypes.swift:3382`, `ChatModel.kt:2944`).
  - For an unverified member it routes to the verification screen (`APIVerifyGroupMember`).
  - The confirmation states that owners cannot be removed and any owner can delete the channel.
- **New `RcvGroupEvent` case** for "you are now an owner", with strings. Both enums are exhaustive, so a missing case throws.
- **First link-changing action by a promoted owner** (profile, relays, promote, leave, delete) shows once that the device connects directly to the channel's link server.
- **Channel link screen.** An owner without a `GroupLink` sees `publicGroup.groupLink`, as subscribers do, and the screen never creates a link for a channel. The screens are `GroupChatInfoView.swift:303-312`, `GroupLinkView.swift:164-166`, `GroupChatInfoView.kt:179-181` and `GroupLinkView.kt:119-123`. Decode `groupLink` in `CRGroupRelaysAdded` and `CEvtGroupLinkDataUpdated` as optional (`AppAPITypes.swift:1001`, `:1210`; `SimpleXAPI.kt:6815`, `:6846`).
- **Leave.** Allow owners to leave channels in both member-status branches per file (`ChatListNavLink.swift:247`, `:272`; `ChatListNavLinkView.kt:322`, `:344`) and in the channel info view. The core's refusal for the last owner is shown as an alert.
- **Relays screen.** For an owner without `group_relays` rows for some relays, it lists the channel's relay members.

---

## Part 8 — version gating and compatibility

- **Relays.** A relay below `multiOwnerVersion` throws on an owner it has no row for (`Store/Groups.hs:2366-2368`), never forwards `x.grp.owner.add`, and refuses an owner's join. So:
  - promotion is refused while any current relay is below it (owners know relays' versions, `supportsVersion`, `Subscriber.hs:933`);
  - adding an older relay to a multi-owner channel leaves it invited and never active; the owner removes it.
- **Members on older versions** ignore `x.grp.owner.add`. They keep rejecting the new owner's admin messages and signed posts until they upgrade, and see its unsigned as-channel posts as channel posts (`Store/Delivery.hs:159`). The profile reaches them through link-data refresh.
- **Joiners on older versions** take every chain entry as an owner, including those in `revokedOwners`. A departed owner's key no longer authorises the link queue, and relays have dropped its connection, so its messages do not reach them.
- **Link servers.** Nothing changes: `LSET`, `LGET`, `RKEY` and `DEL` exist on every server that hosts channel links.

---

## Part 9 — tests (scope)

Under `describe "channels"` (`tests/ChatTests/Groups.hs`), with relays from `chatRelayProfile`:

- **Promotion.**
  - O1 promotes verified M. A subscriber that joined before accepts M's admin message after `x.grp.owner.add`. A member that knew M with a different relay-asserted key ends with the chain key.
  - An unverified member's promotion is refused.
  - A promotion is refused while a relay is below `multiOwnerVersion`.
  - M's device ignores a commit whose `ownerKey` is not its own key.
- **Creator hand-over.** The creator promotes M and leaves. Then M:
  - posts to every subscriber through every relay;
  - edits the profile, with existing subscribers and a new joiner seeing it;
  - adds a relay, which becomes active, existing subscribers get `XGrpRelayNew`, and a subscriber connected only to it receives M's post;
  - removes a relay, which is dropped from the link and stays dropped after M's next write;
  - promotes a verified subscriber;
  - destroys the channel, after which the link no longer resolves.

  A joiner after the leave sees M as owner and the creator as left. It verifies the stored roster and receives the creator's signed as-channel history posts.
- **Three owners.** The creator promotes M (root-signed entry). M promotes N (entry signed by M).
  - M leaves: M's entry stays in the chain as N's signer, link writes by N and the creator keep validating, and joiners treat M as left.
  - N then leaves: the next write drops both entries.
- **Admin to owner.** An admin promoted to owner stays owner on relays and a later joiner after the stored roster is served.
- **Relays added by another owner.** O2 adds relay R3; O1 joins R3 as an owner from `XGrpRelayNew` and from reconcile, with its membership and profile unchanged afterwards.
- **Concurrent writes.**
  - Two owners edit the profile at the same moment: both writes return, and one profile is published.
  - Two owners add relays at the same moment: both relays end published after the verify.
- **Concurrent roles.** Two owners change roles of different members at one version: every relay serves the same blob holding both after the re-send, and every member applies both.
- **Healing.** A promoter stopped after its link write and before sending the commit, by a test hook: another owner's reconcile announces M, and M becomes owner.
- **Replay.** A relay replays an old `x.grp.owner.add` for an owner that left: members and a later joiner ignore it.
- **Leave.**
  - The last owner's leave is refused.
  - The creator's leave sends no `DEL`, and the link still resolves.

The concurrent cases use forced interleaving (`deliveryWorkerDelay`, a test hook between read and write). simplexmq tests are listed in Part 1.

---

## Later

Deferred parts, with the overview section that describes the limitation:

- **Owner removal** (`RKEY` first, then `revokedOwners`, verify, `x.grp.mem.del`), with a server compare-and-swap on `LSET`/`RKEY` to close its race with a concurrent promotion (§11).
- **Promotion over a direct contact**, with the member's consent (§6, §11).
- **Relays sending a new owner their member list** (§6).
- **A relay-sourced subscriber count** (§5).
- **Multi-writer roster convergence** and carrying a departing owner's last changes (§4).
- **Field-level profile merge** and a link-data version for stale profile events (§2).
- **In-order relay forwarding, and queueing messages to backlogged relays** (single-owner fixes, §11).
- **The creator retiring the root key** after its first promotion (§7).
