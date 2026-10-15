> Superseded by the S3-min plans: `2026-10-15-channel-multi-owner-s3min.md` (overview) and `2026-10-15-channel-multi-owner-s3min-implementation.md` (implementation).

# Channel multi-owner — implementation plan

Companion to the design overview (`2026-07-15-channel-multi-owner.md`); read that first for *why*. This covers *what to build*: agent APIs, data model, types, the protocol event, commands, logic, sequencing. It is a map for implementing agents, not an exhaustive edit list; trivial wiring (JSON instances, `CMEventTag` cases, view strings) is implied.

In short:

- **Owners.** A channel has several equal owners. Any owner can promote a member whose security code it has verified, and any owner can leave while another remains.
- **No SMP server change.**
  - A promoted owner writes the link with today's `LSET`, resending the fixed data it read with `LGET`.
  - It gets write access through today's `RKEY`. `RKEY` was added with short links (simplexmq `b7a95422`, SMP v15), so every server that hosts a channel link accepts it.
- **Link writes.** Every link write in a multi-owner channel reads the link data, changes only its own part, writes, and re-reads after owner and relay changes.
- **Link data is the durable source of owners and departed owners.** Every party applies it when it reads it.
- **The roster** keeps today's single-writer protocol on members and relays.

## Sequencing

Two repositories, in order:

1. **simplexmq client and agent (Part 1).** No server or protocol change, and no agent schema change.
2. **simplex-chat (Parts 2–8).**

The relay side ships in the same release as promotion. Promotion is refused while any current relay is below `multiOwnerVersion` (Part 8).

---

## Part 1 — simplexmq (client and agent)

- **SMP client.**
  - Add `setSMPQueueRcvKeys`, which sends `RKEY` signed with a given recipient private key. Today the client has no `RKEY` call; the server handles it at `Server.hs:1574`.
  - `LSET` (`addSMPQueueLink`, `Client.hs`) and `DEL` (`deleteSMPQueue`) already take an explicit key and queue ID.
- **`LinkOwnerCreds`**, a new agent type, holds what a non-creator owner needs to write a link:
  - `SMPServer` and `LinkKey`, from the channel link;
  - `linkRcvId`, from the link data read at the start of each write;
  - the root public key;
  - the owner's member private key, which both signs the user data and authorises the queue commands.

  Chat assembles it from stored values and the write's own read. The agent stores nothing.
- **Read for writing.** `getConnShortLinkForWrite` is `getConnShortLink'` (`Agent.hs:1321-1350`) with three differences:
  - it also returns the encrypted fixed data from the `LGET` response (`getQueueLink`, `Agent/Client.hs:2015-2020`);
  - it returns the raw user-data JSON, so the caller can keep fields it does not know;
  - when the read is not proxied, it uses the same per-link transport session as the writes below.

  `LSET` requires the fixed data byte-identical to the stored copy (`QueueStore/STM.hs` `addQueueLinkData`; Postgres the same), and `LGET` returns it as stored (`Server.hs:1671-1672`).
- **Write with credentials.** `setLinkDataWithCreds creds encFixedData userContactData`:
  1. builds `ShortLinkCreds {shortLinkId, shortLinkKey, linkPrivSigKey = owner key, linkRootSigKey = Just root, linkEncFixedData}` in memory;
  2. runs `validateOwners`, which requires the owner key in the chain (`Agent/Protocol.hs:1991-2001`);
  3. signs and encrypts the user data as `prepareContactLinkData` does (`Agent.hs:1239-1272`), with encoding and size check separable, so a caller can check the size before any key change;
  4. sends `LSET` on `linkRcvId`, signed with the owner key.

  `setLinkRcvKeysWithCreds` (`RKEY`) and `deleteLinkQueueWithCreds` (`DEL`) take the same credentials. All three are synchronous and run on a transport session of their own per link: `TSMEntity` with the link's recipient ID, where the default session is shared per user and server (`mkTSession`, `Agent/Client.hs:1442-1443`).
- **Creator's own queue.**
  - `getConnLinkQueue connId` returns the link queue's recipient ID and the public half of its `rcvPrivateKey`. It is a local read.
  - `setConnLinkRcvKeys connId keys` sends `RKEY` on the creator's own link queue, signed with its stored key.
  - The creator's link-data writes keep using `setConnShortLink` (`Agent.hs:1239-1250`), which already resends the stored encrypted fixed data.
- **Local delete.** `deleteConnectionLocal connId` deletes a connection and its queue records, and removes its subscriptions as `prepareDeleteConnections_` does (`Agent.hs:3031-3032`), without any server command. Every agent deletion path today sends `DEL` (`deleteConnectionsAsync_`, `Agent.hs:1022-1030`).
- **`Crypto/ShortLink.hs`.** Extract `mkOwnerAuth :: OwnerId -> PublicKeyEd25519 -> PrivateKeyEd25519 -> OwnerAuth`, define `newOwnerAuth` with it, and export it. Chat signs another member's existing key with it.
- **Forward compatibility.** The `UserContactData` parser drops trailing fields it does not know (`Agent/Protocol.hs:2075-2079`). A future agent-level link field therefore needs a version gate, or an older owner's write erases it.
- **Tests.**
  - Link data written with a non-root owner key is accepted by a reader.
  - `RKEY` with credentials lets the added key write and stops the removed key.
  - `DEL` with credentials deletes the queue.
  - `getConnShortLinkForWrite` followed by `setLinkDataWithCreds` keeps the fixed data.
  - `deleteConnectionLocal` sends nothing to the server.

---

## Part 2 — simplex-chat: data model

- **No migration and no table changes.**
  - Owners are member rows with role owner, and their keys are in `member_pub_key`.
  - Departed owners are rows with status `GSMemLeft`, kept for good (Part 6.11).
  - A promoted owner stores no link credential. Each of its link writes builds `LinkOwnerCreds` from four sources: the server and `LinkKey` from the channel link, the group's root public key, its member private key, and the `linkRcvId` of that write's own read.
- **Link data.** Chat's `GroupShortLinkData` (`Protocol.hs:1621`) gains three optional fields. simplexmq's `UserContactData` encoding is unchanged.
  - `revokedOwners :: Maybe [MemberId]`: owners that left.
  - `creatorQueueKey :: Maybe CreatorQueueKey {memberId, key}`: the creator's memberId and link-queue recipient public key.
  - `linkRcvId`: the link queue's recipient ID.
- **Multi-owner channel.** A query that is true when this device holds an owner row other than its own membership, current or left. It selects the read-merge write path (Part 6.1). Owner rows are never deleted in channels (Part 6.11).

---

## Part 3 — types

- `LinkOwnerCreds` (simplexmq, Part 1).
- **`GroupLinkWriter = GLWCreator Connection | GLWOwner LinkOwnerCreds`.** It is resolved once per link write, from the keys:
  - the root private key (`GRKPrivate`) selects the creator's own link connection;
  - otherwise, if this device is a current owner, it selects the credentials, with `linkRcvId` from the write's own read;
  - otherwise the device is not a link writer.

  It is never resolved from whether a group link row exists.
- **`CRGroupRelaysAdded` and `CEvtGroupLinkDataUpdated`** (`Controller.hs:862`, `:1035`) carry `Maybe GroupLink`. It is absent for a promoted owner, which has no group link row.
- **`GroupOwnerAdd {memberId, ownerAuth, profile}`** is the payload of `x.grp.owner.add`. `OwnerAuth` is reused from simplexmq.
- **"Current owner"** everywhere below means owner role, and neither left nor removed.

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

- **`APIMembersRole [M] GROwner` on a channel** starts the promotion (Part 6.3), replacing the ban at `Commands.hs:2994-2995`. It is refused when any of these holds:
  - more than one target;
  - a target that is not a current member with a key, is already an owner, or is in `revokedOwners`;
  - the promoter has not verified the target's security code: `memberVerifiedCode` must equal `getChannelMemberCode` now (`Commands.hs:3811-3815`, stored by `setGroupMemberVerified`, `Store/Groups.hs:1754-1760`);
  - any current relay below `multiOwnerVersion`.

  The target's version is not checked. The promoter only knows the range it recorded at the member's join (`Store/Groups.hs:2533-2551`).
- **`APIMembersRole` or `APIRemoveMembers` with a current owner as a target is refused in channels.**
- **`APIRemoveMembers` of a relay**, by any link writer, publishes the removal through Part 6.1 with that relay subtracted. Today it publishes only through the creator's `updatePublicGroupData` (`Commands.hs:3150-3152`).
- **`APILeaveGroup` by an owner of a multi-owner channel** runs the leave (Part 6.6) instead of `deleteGroupLinkIfExists` (`Commands.hs:3246`).
  - It is refused when no other current owner remains.
  - A sole owner keeps today's behaviour.
- **`APIDeleteChat` by a promoted owner**, in the branch that sends `x.grp.del` (`doSendDel`, `Commands.hs:1394-1400`), also deletes the link queue with `deleteLinkQueueWithCreds`, after reading the link data for its `linkRcvId`. `deleteGroupLinkIfExists` at `:1400` runs regardless of `doSendDel`, so this `DEL` is not added there. When this device created the channel and its membership is no longer a current owner (a leave stopped after step 5), `APIDeleteChat` deletes the link connection with `deleteConnectionLocal` instead of `deleteGroupLinkIfExists`. The key may still be authorised (overview §3), so it never sends `DEL`.
- **`APICreateGroupLink`, `APIAddGroupShortLink` and `APIDeleteGroupLink` are refused for channels** (`Commands.hs:3349-3384`).
- **`APIGetUpdatedGroupLinkData`.** Replace the owner branch at `Commands.hs:1968` with the owner reconcile (Part 6.2). For subscribers it also applies the owners chain (Part 6.5).
- **`APIAddGroupRelays`.** A promoted owner takes the channel link from `publicGroup.groupLink`; the creator takes it from its group link row (`Commands.hs:2790-2793`). Publication goes through Part 6.1.
- **`APIShareChatMsgContent`** signs with the member key of any current owner, not only under `GRKPrivate` (`Commands.hs:1238-1240`).
- **The connect plan** excludes `revokedOwners` from `glOwners`, from `verifyLinkOwner` and from owner-contact binding (`Commands.hs:4615-4618`, `:4648-4653`, `:4806-4815`, `:2327-2332`).

---

## Part 6 — key logic changes (anchors)

### 6.1 Link writes in a multi-owner channel

One function, `writeChannelLink`, takes the change: profile, add owner, leave, relays added or removed, or count. All of a device's link writes for a channel run under the group lock.

1. **Read.** The creator reads with `getShortLinkConnReq'` (`Internal.hs:1690-1693`), keeping the raw user-data JSON. A promoted owner reads with `getConnShortLinkForWrite`, which also gives it the `linkRcvId` for this write.
2. **Build the write** from the read JSON, so fields this version does not know are kept, applying only this change (overview §2):
   - the profile as a whole value;
   - a new owner's entry appended after its signer's entry;
   - the writer's own memberId added to `revokedOwners`, keeping its own entry;
   - relays: the read list, plus the relays this call adds, minus the relays this call removes, minus relays whose member row is left. A relay removal deletes its member row (`deleteOrUpdateMemberRecordIO`, `Internal.hs:2195-2196`), so the removed relays are passed in.

   Then merge:
   - owners this device holds as left go into `revokedOwners`;
   - an owner held as current, missing from the chain and not in `revokedOwners` gets an entry signed by the writer;
   - an entry in `revokedOwners` is dropped once no current entry was signed by it, transitively, except the writer's own entry in its leave write. Keep the chain in signing order, never a memberId sort: `validateLinkOwners` needs each entry signed by root or an earlier entry (`Agent/Protocol.hs:2003-2017`);
   - the creator writes its own `creatorQueueKey` and `linkRcvId`, from `getConnLinkQueue`; every other writer carries the read ones; every writer carries `publicGroupData`.
3. **Write** through the resolved `GroupLinkWriter`: the creator uses `setConnShortLink`, a promoted owner uses `setLinkDataWithCreds`.
4. **Keys.** If the chain changed, send `RKEY` with the set computed from what was written. A promotion sets the keys first instead (Part 6.3). The set (overview §3) is:
   - `creatorQueueKey.key`, unless `creatorQueueKey.memberId` is in `revokedOwners`;
   - plus the `ownerKey` of every other entry not in `revokedOwners`.
5. **Verify.** After an owner, leave or relay change, read again. If the change is missing, repeat once from step 1.

**Relay activation.** The body of the `LINK` handler (`Subscriber.hs:1431-1481`) becomes a function of the written relay list and the writer. It sets relays to `RSActive`, sends `XGrpRelayNew` to the other relays, and emits `CEvtGroupLinkDataUpdated`. It is called after step 5, because these writes are synchronous and produce no `LINK` event. Today the handler re-reads the group link row (`:1439`), which a promoted owner does not have.

**Callers in a multi-owner channel:**

- the profile edit, which today calls `setGroupLinkData'` (`Commands.hs:4190`, `Internal.hs:1581-1586`);
- the relay acknowledgement (`xGrpRosterAck`, `Subscriber.hs:3663-3665`) and the relay connect path for pre-roster relays (`Subscriber.hs:946-947`), which today use `setGroupLinkDataAsync`;
- relay removal (Part 5) and a relay's own leave (Part 6.10);
- promotion, leave, and the creator's count publish.

Asynchronous callers run the synchronous write in a thread, still under the group lock. A failed write is not retried (overview §8). A single-owner channel keeps today's functions unchanged.

### 6.2 Owner reconcile

It runs in `APIGetUpdatedGroupLinkData` for owners, and once after chat start for each multi-owner channel the user owns. From the read link data:

- **Profile.** Apply it as subscribers do (`updateGroupFromLinkData`, `Internal.hs:1634-1654`).
- **Owners chain.** Apply it as in Part 6.5.
- **Relays.** Join each listed relay this owner has no relay member for, through `connectToRelayAsync` (`Internal.hs:1606-1616`). Unlike `syncSubscriberRelays` (`Commands.hs:3992-4011`), this deletes nothing, so a relay this owner is still adding is kept.
- **Count.** The creator publishes its count if it differs (Part 6.10).
- **Roster.** At chat start only, and only when the newest item this device received in the channel is older than message expiry, or there is none, send each relay `XGrpRosterRequest` with this owner's complete frontier (Part 6.9 rule 7).

### 6.3 Promotion

Under the group lock, synchronously:

1. **Read and prepare.** Read the link data and build the write with M's entry (Part 6.1 step 2).
   - The creator signs the entry with the root key, as `Commands.hs:2757` does for itself; any other owner signs it with its member key. Both use `mkOwnerAuth` over M's stored key.
   - On the creator's first promotion the write also adds `creatorQueueKey` and `linkRcvId`, both from `getConnLinkQueue`.
   - Encode, sign and size-check the write now, for a promoted owner (`setLinkDataWithCreds`, Part 1). A write that would not fit in 13,784 bytes (`ShortLink.hs:46`) fails here. The creator's `setConnShortLink` checks the size only when it sends (`Agent.hs:1236-1270`), so the creator's oversize write fails at step 3, after its `RKEY`.
2. **Keys.** `RKEY` with the set computed from what was read, plus M's member key, plus the creator's own queue key on its first promotion. The creator uses `setConnLinkRcvKeys`; any other owner uses `setLinkRcvKeysWithCreds`.
3. **Write** the link data.
4. **Verify**, and repeat once from step 1 if M's entry is missing. A failure after step 2 fails the command without a revert.
5. **Commit.** Set M's role to owner locally, and send `x.grp.owner.add {memberId, ownerAuth, profile}` to the current relays. The profile is `redactedMemberProfile` (`Internal.hs:1361`), as member intros send it.

### 6.4 Applying `x.grp.owner.add`

On relays (`processEvent`) and on members (`processForwardedMsg`). Accept it only when all of these hold:

- the sender is a current owner (signed; the event is in `requiresSignature`);
- `ownerAuth.ownerId == memberId`;
- `ownerAuth` verifies against the group's root public key (`groupRootPubKey`) or an owner key this device holds, current or left, because a delegated entry stays valid after its signer leaves;
- this device holds no row for `memberId` that left or was removed.

Then:

- set role owner and `member_pub_key = ownerAuth.ownerKey`, logging a warning when it replaces a different key;
- if the member is unknown, create it with `profile`; if its profile is a placeholder, update it;
- on a relay, return the group delivery scope so the event is forwarded.

On M's own device, the commit completes M's promotion as in Part 6.5.

### 6.5 Owners from link data

`updateGroupFromLinkData` (`Internal.hs:1634-1654`; callers `Commands.hs:1966`, `:4624`, `:4651`) also receives the link's owners and `revokedOwners`, and applies them on every party that reads the link data. The agent has already validated the chain against the root key (`decryptLinkData`, `ShortLink.hs:114-116`).

- **Departed owners.** For each memberId in `revokedOwners`, mark the member left, at whatever role this device holds it: a device that missed the commit holds it at its earlier role. A relay also drops that member's connection, as `xGrpLeave` does (`Subscriber.hs:3815-3818`). If the device holds no row, create a row of a member that left. This device's own membership is never changed. When it is listed, a leave of this device stopped after its link write, and leaving again continues it (Part 6.6).
- **New owners.** For each other chain entry whose member this device holds neither as owner nor as left or removed, set that member as owner with `ownerKey`, creating it with a placeholder profile if unknown.
- **This device's own entry.** If the chain lists this device's own memberId, not in `revokedOwners`, with its own member key, and its membership is current, it becomes owner:
  - set the membership role to owner;
  - create the "you are now an owner" service item.

  It stores nothing else: each of its link writes takes `linkRcvId` from its own read (Part 6.1).

  The commit (Part 6.4) does the same on receipt. An entry with any other key is ignored with an alert.

**At join.** `APIConnectPreparedGroup` (`Commands.hs:2324-2332`) creates owners only for entries not in `revokedOwners`, and creates rows of members that left for memberIds in `revokedOwners`.

### 6.6 Leave

Under the group lock, synchronously:

1. Read the link data and merge it as Part 6.1 step 2 does, which re-adds a current owner missing from the chain. Refuse unless the merged data shows another current owner: a chain entry other than its own, not in `revokedOwners`. If its own memberId is already in `revokedOwners`, this is a repeated leave: go to step 4.
2. Write with its own memberId in `revokedOwners`, keeping its own entry.
3. Verify, and repeat once if needed.
4. `RKEY` with the set computed from what was written, which no longer includes its own key. An `AUTH` error counts as done: its key is already gone.
5. Send `x.grp.leave` and delete member connections, as `APILeaveGroup` does today (`Commands.hs:3233-3253`).
6. Erase link credentials and keys:
   - the creator deletes its link connection with `deleteConnectionLocal` and deletes its group link row;
   - the creator replaces `root_priv_key` with `root_pub_key` in one update, so the group keeps its root public key;
   - every leaver erases `member_priv_key`. A later load of the group generates an unrelated key (`mkGroupInfoKeys`, `Store/Shared.hs:750-755`), so the erased key is gone.

**On receivers.** `xGrpLeave` (`Subscriber.hs:3815-3826`) is unchanged for members and relays. A remaining owner also re-sends its roster blob (Part 6.9 rule 3).

### 6.7 Relays

- **Invitation.** `updateRelayGroupKeys` (`Store/Groups.hs:2353-2373`, called from `getLinkDataCreateRelayLink`, `Subscriber.hs:4586-4612`) creates an owner row with `ownerKey` for every chain owner it has no row for, as `createLinkOwnerMember` does for joiners; today it throws at `:2366-2368`. It applies `revokedOwners` (Part 6.5). The inviting-owner check (`Subscriber.hs:4598-4599`) also refuses an owner in `revokedOwners`.
- **15-minute check.** `checkRelayServedGroups` (`Commands.hs:6001-6020`) already reads the link data; it also applies the owners chain and `revokedOwners` (Part 6.5).
- **Owner join.** In `memberJoinRequestViaRelay` (`Subscriber.hs:1690-1720`), an existing owner row is accepted like a roster-role row:
  - the join is signed with that row's key;
  - it passes the `viaRelay` check (`:1711-1714`);
  - `acceptJoin` runs with the existing row and role owner.

  A join onto a row whose member left or was removed is refused in both branches. Today the roster branch checks the key but not the status (`:1702-1705`). The relay then treats the joining owner as any joiner, including sending it recent history.
- **History for joiners.** In `itemForwardMsgs` (`Internal.hs:1449-1460`, `:1540-1548`), a signed as-channel item whose author is no longer a current owner is sent as an unsigned channel item (`FwdChannel`), with its content re-encoded as for unsigned items.

### 6.8 Owner joining a relay

An owner joins a relay through `connectToRelayAsync`, from `xGrpRelayNew` (`Subscriber.hs:3881-3886`) and from the reconcile (Part 6.2). When the join completes, only the relay member is updated:

- `updatePreparedUserAndHostMembers'` (`Store/Groups.hs:821-844`) skips the membership update and the group profile replacement when the membership role is owner;
- the `CON` branch for a host member (`Subscriber.hs:892-922`) skips the membership status update and the joined-channel items and event.

### 6.9 Roster rules

Members and relays keep `applyAtRosterVersion` and the completion gate. The rules from overview §4:

1. **Tie-break.** In `rosterCompletion` (`Subscriber.hs:3572-3578`), when `pendingVer` equals `stored_roster_version`, keep the stored blob unless the incoming author's memberId is at least the stored author's (from `roster_sending_owner_gm_id`, `getStoredGroupRoster`, `Store/Groups.hs:1615-1630`). Below or above the stored version, today's gate decides.
2. **Concurrency re-send.** When an owner applies another owner's role or removal event whose version equals its gate (in `applyAtRosterVersion`, `Subscriber.hs:3404-3446`), then after the whole received message is processed it re-sends its current roster to every current relay at gate+1, as a blob only. It reserves that version as `broadcastRoster` does (`Internal.hs:2642-2645`) and sends with `sendRoster` (`Internal.hs:2665-2670`) and `buildGroupRoster` over the current roster members.
3. **Departure re-send.** When this device is an owner, it re-sends the same way, at gate+1, in two cases:
   - it applies another owner's departure, either from `xGrpLeave` of an owner, or from Part 6.5 marking a held owner left from `revokedOwners`;
   - it applies a served blob whose author it holds as left (rule 6).

   A departure from a link-data read can come before the departed owner's last forwarded messages, which `xGrpMsgForward` then drops (`Subscriber.hs:4019-4020`). So on that path the owner first sends `XGrpRosterRequest` with its complete frontier to every current relay, before its own re-send. The request then carries the frontier from before that re-send.
4. **Owners are outside the roster.**
   - `processRosterEntries` (`Subscriber.hs:3610-3630`) skips entries for current owners and for members held as removed or left.
   - `xGrpMemRole` (`Subscriber.hs:3448-3500`), including its own-membership branch (`:3451-3454`), ignores an event whose target is a current owner or a member held as removed or left, and any event that assigns the owner role.
   - `xGrpMemDel` (`Subscriber.hs:3731-3795`), including its own-membership branch, ignores an event whose target is a current owner.
5. **Own versions advance the frontier.** `broadcastRoster` and the rule-2 and rule-3 re-sends set the complete frontier to the reserved version when it is the next one (`setCompleteRosterVersion`, as `Subscriber.hs:3419-3421` does on receipt).
6. **In sync before sending.** An owner sends a blob (own change, rule 2, rule 3) only when its complete frontier equals its gate. While it is out of sync:
   - it sends `XGrpRosterRequest` with its complete frontier to every current relay, and again with each role change it makes;
   - `broadcastRoster` reserves the version and sends the events without the blob;
   - rules 2 and 3 wait until it is back in sync.

   Every owner, in sync or not, processes in `xGrpMsgForward` (`Subscriber.hs:4009-4027`) an `XGrpRoster` whose author it holds as left, verified against that author's kept key, instead of ignoring it. Completion still rejects a version below its gate. This is how an owner catches up both when it is out of sync and after a departure read from link data (rule 3).
7. **Absence request.** At chat start, when the newest item an owner received in the channel is older than message expiry, or there is none, it sends `XGrpRosterRequest` with its complete frontier to each relay. Relays already answer members this way (`xGrpRosterRequest`, `Subscriber.hs:3679-3685`).

### 6.10 Count, and a relay's own leave

In `updatePublicGroupData` (`Internal.hs:1620-1630`):

- The owner branch runs only for the creator, recognised by `GRKPrivate`. For a promoted owner it does nothing; today it would throw on the missing group link row.
- In a multi-owner channel the creator's branch recounts but does not publish on joins, leaves and removals of subscribers.
- When the member that left is a relay, the creator publishes through Part 6.1 with that relay subtracted.
- The creator's reconcile publishes the count when it differs from the published one.
- Every other write carries the read count.

### 6.11 Durable owner rows

In channels:

- **Rows are kept.** `getGroupMembersForExpiration` (`Store/Groups.hs:1357-1373`, used by item expiry `Commands.hs:6078-6079` and Clear chat `:1429-1430`) and `getRemovedMembersToCleanup` (`Store/Groups.hs:1375-1382`) exclude member rows whose role is owner, admin, moderator or member.
- **Intros don't revive.** `xGrpMemIntro` (`Subscriber.hs:3314-3322`) leaves the status of an existing member held as left or removed unchanged. Today `updatePreparedChannelMember` sets it to `GSMemIntroduced` (`Store/Groups.hs:3608-3623`).

---

## Part 7 — apps (iOS and Kotlin, mirrored)

- **Role picker.** Add Owner for channels (`canChangeRoleTo`, `ChatTypes.swift:3382`, `ChatModel.kt:2944`).
  - For an unverified member it routes to the verification screen (`APIVerifyGroupMember`).
  - The confirmation states that owners cannot be removed, that any owner can delete the channel, and that a member with an older app becomes an owner after it upgrades.
  - Remove and role change are hidden for owner targets in channels (`ChatTypes.swift:3377-3390`, `ChatModel.kt:2938-2953`).
- **New `RcvGroupEvent` case** for "you are now an owner", with its strings. Both enums are exhaustive, so a missing case throws.
- **First link-changing action by a promoted owner** (profile, relays, promote, leave, delete) shows, once, that the device connects directly to the channel's link server.
- **Channel link screen.**
  - An owner without a `GroupLink` sees `publicGroup.groupLink`, as subscribers do.
  - The screen never creates a link for a channel: `GroupChatInfoView.swift:303-312`, `GroupLinkView.swift:164-166`, `GroupChatInfoView.kt:179-181`, `GroupLinkView.kt:119-123`.
  - Decode `groupLink` in `CRGroupRelaysAdded` and `CEvtGroupLinkDataUpdated` as optional (`AppAPITypes.swift:1001`, `:1210`; `SimpleXAPI.kt:6815`, `:6846`).
- **Leave.** Allow owners to leave channels, in both member-status branches per file (`ChatListNavLink.swift:247`, `:272`; `ChatListNavLinkView.kt:322`, `:344`) and in the channel info view. The core's refusal for the last owner shows as an alert.
- **Relays screen.** For an owner without `group_relays` rows for some relays, it lists the channel's relay members.

---

## Part 8 — version gating and compatibility

- **Relays below `multiOwnerVersion`** throw on an owner they have no row for (`Store/Groups.hs:2366-2368`), never forward `x.grp.owner.add`, and refuse an owner's join. So:
  - promotion is refused while any current relay is below it (owners know relays' versions, `supportsVersion`, `Subscriber.hs:933`);
  - adding an older relay to a multi-owner channel leaves it invited and never active, and the owner removes it.
- **Members on older versions** ignore `x.grp.owner.add`.
  - They keep rejecting the new owner's admin messages and signed posts until they upgrade.
  - They see its unsigned as-channel posts as channel posts (`Store/Delivery.hs:159`).
  - The profile reaches them through link-data refresh.
- **A target on an older version** ignores the commit. It becomes an owner after it upgrades and reads the link data (Part 6.5).
- **Joiners on older versions** take every chain entry as an owner, including those in `revokedOwners`.
  - A departed owner's key no longer authorises the link queue, and relays have dropped its connection, so its messages do not reach them.
  - They also apply a relay's stored blob made before an admin's promotion, and show that owner as admin.
- **Link servers.** Nothing changes: `LSET`, `LGET`, `RKEY` and `DEL` exist on every server that hosts channel links.

---

## Part 9 — tests (scope)

Under `describe "channels"` (`tests/ChatTests/Groups.hs`), with relays from `chatRelayProfile`:

- **Promotion.**
  - O1 promotes verified M. A subscriber that joined before accepts M's admin message after `x.grp.owner.add`. A member that knew M with a different relay-asserted key ends with the chain key.
  - An unverified member's promotion is refused.
  - A promotion is refused while a relay is below `multiOwnerVersion`.
  - M's device ignores a commit or chain entry whose key is not its own.
- **Creator hand-over.** The creator promotes M and leaves. Then M:
  - posts to every subscriber through every relay;
  - edits the profile, with existing subscribers and a new joiner seeing it;
  - adds a relay, which becomes active, existing subscribers get `XGrpRelayNew`, and a subscriber connected only to it receives M's post;
  - removes a relay, which is dropped from the link and stays dropped after M's next write;
  - promotes a verified subscriber;
  - destroys the channel, after which the link no longer resolves.

  A joiner after the leave sees M as owner and the creator as left, verifies the stored roster, and receives the creator's as-channel history posts, served unsigned. The same holds after M clears the chat and after item expiry: M still edits the profile and adds a relay, and its leave as last owner is refused.
- **Three owners.** The creator promotes M (root-signed entry), and M promotes N (entry signed by M).
  - M leaves: M's entry stays in the chain as N's signer, link writes by N and the creator keep validating, and joiners treat M as left.
  - N then leaves: the next write drops both entries.
- **Admin promoted to owner.** It stays owner on relays, and on a later joiner after the stored roster is served.
- **Relays added by another owner.** O2 adds relay R3, and O1 joins R3 as an owner, both from `XGrpRelayNew` and from reconcile. O1's membership and profile are unchanged afterwards.
- **Concurrent writes.**
  - Two owners edit the profile at the same moment: both writes return, and one profile is published.
  - Two owners add relays at the same moment: both relays end published after the verify.
- **Concurrent roles.**
  - Two owners change roles of different members at one version: after the gate+1 re-send, every relay serves the same blob holding both, every member applies both, and a joiner gets both.
  - An owner with a gap whose blob author has left asks every relay, applies the served blob, and only then sends a blob, so a removal made just before the leave stays on relays.
  - A relay withholds a delta from an out-of-sync owner, and no relay holds a newer blob. The owner still demotes an admin: the event reaches every relay and member, and no blob is sent.
- **Lost commit.** A promoter is stopped after its link write and before sending the commit, by a test hook. A member and a relay set M as owner from the link data (on open, and in the 15-minute check), and M becomes owner from its own entry and writes the link.
- **Stale single-owner write.** A single-owner link write queued before the creator's first promotion lands after it. M's writes fail until the creator's next write, which restores M's entry, `creatorQueueKey` and `linkRcvId`. Then M writes, and so does an owner M promotes after the creator leaves.
- **Departed owners.**
  - A relay replays an old `x.grp.owner.add` for an owner that left, to a member that knew the leave, to one that never knew M, and to a later joiner: all ignore it once they have read the link data.
  - An intro of a departed owner leaves it left.
  - A relay that withholds the leave: the member marks the owner left on its next channel open. So does a member that held the departed owner as a subscriber, because it missed the commit.
- **Leave.**
  - The last owner's leave is refused.
  - The creator's leave sends no `DEL`, and the link still resolves.
  - A leave stopped after step 2 and repeated continues at step 4. A reconcile before the repeat leaves the leaver's own membership current. A creator that deletes the chat after its leave stopped after step 5 sends no `DEL`.
  - The creator changes roles while M is offline, then leaves. M reads the link data before its backlog: it asks every relay, applies the creator's served blob, and re-sends, and a later joiner gets a roster it can verify.
  - After any leave, the leaver's stored member key no longer matches its chain entry's key.

The concurrent cases use forced interleaving (`deliveryWorkerDelay`, a test hook between read and write). simplexmq tests are listed in Part 1.

---

## Later

Deferred parts, with the overview section that describes the limitation:

- **Owner removal** (`RKEY` first, then `revokedOwners`, verify, `x.grp.mem.del`), with a server compare-and-swap on `LSET`/`RKEY` to close its race with a concurrent promotion and the races of §2 and §3 (§11).
- **Promotion over a direct contact**, with the member's consent (§6, §11).
- **Relays sending a new owner their member list** (§6).
- **A relay-sourced subscriber count** (§5).
- **Multi-writer roster convergence**, and carrying a departing owner's last changes (§4).
- **Field-level profile merge**, and a link-data version for stale profile events (§2).
- **A removed-relay tombstone** (§2).
- **In-order relay forwarding, and queueing messages to backlogged relays** (single-owner fixes, §11).
- **The creator retiring the root key** after its first promotion (§7).
