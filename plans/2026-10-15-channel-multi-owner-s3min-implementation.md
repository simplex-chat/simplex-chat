# Channel multi-owner (S3-min) — implementation plan

Run: 004-channel-multi-owner-s3min · Branch: `f/channel-multi-owner` · Repos: simplexmq (from pinned `cba13823`), simplex-chat · Estimated diff: ~2,100 production Haskell lines (simplexmq ~450, chat ~1,650) plus tests, and ~350 lines per app platform.

The overview, `2026-10-15-channel-multi-owner-s3min.md`, says what owners can do and why; this file says what to build. Anchors are simplex-chat paths under `src/Simplex/Chat/` and simplexmq paths under `src/Simplex/Messaging/` at `cba13823`.

## 1. Goal and acceptance criteria

A channel has several equal owners. Any owner can do what the single owner does today; owners are added with the member's consent, can remove each other and can leave; concurrent owner actions are ordered by the link server, so none is lost, overwritten unseen or reverted.

| # | Criterion | Test (§7) |
|---|---|---|
| AC1 | An owner invites a member whose code it verified; the member accepts; one link write adds the member's chain entry and recipient key; every relay and subscriber treats it as owner; it posts, edits and changes roles | T1 |
| AC2 | Promotion is refused for an unverified member, while a relay is below `multiOwnerVersion`, and on a link server below SMP v24 | T2 |
| AC3 | The creator promotes M and leaves; M does every owner action (post, profile, add and remove relays, roles, promote, remove, destroy); a later joiner sees M as owner and the creator as departed, verifies the roster and gets the creator's history unsigned | T3 |
| AC4 | An owner removes another; the removed owner's link writes and `DEL` get `AUTH`; its device shows the removal and erases its keys | T4 |
| AC5 | Two owners writing the link at once both land (one after a conflict); a profile edit over a changed profile is refused; role changes by two owners at once reach every relay and member and no demotion is reverted; mutual removal and the last two owners leaving at once each leave one current owner | T5–T8 |
| AC6 | A role change made just before its author leaves reaches every device: relays and owners accept the departed author's committed blob, and a remaining owner re-signs | T9 |
| AC7 | A new owner pulls the member list page by page from one relay; the count keeps updating after the creator leaves | T10, T11 |
| AC8 | An older profile event or link read never replaces a newer profile | T12 |
| AC9 | `LSWP` swaps link data and keys atomically on the STM, journal and Postgres stores, and a restart replays both | S1–S3 |
| AC10 | Single-owner channels behave as today: the existing `-m "channels"` suite passes | §7 |

## 2. Non-goals

Retiring or rotating the root key; protection against a malicious current owner; ordering block and unblock; relay forwarding order and queueing to backlogged relays (single-owner fixes); field-level profile merge; equal-version roster tie-break; a decline, withdrawal or cancel of owner invitations; profile images in the member catch-up; behaviour of older apps beyond failing closed.

## 3. Current state

Single-owner assumptions this plan replaces, verified in code (full list in `research.md` of the run):

| Where | Today |
|---|---|
| `Internal.hs:1669-1685` `groupLinkData` | owners = `[self]`, only with the root private key |
| `Internal.hs:1618-1631` `updatePublicGroupData` | creator republishes the count per join or leave; throws without a group link row |
| `Commands.hs:2994-2995` | owner role refused in channels |
| `Commands.hs:3246`, `Internal.hs:1739-1748` | leaving deletes the link queue with `DEL` |
| `Internal.hs:2642-2654` `broadcastRoster` | roster version = local gate + 1, single signer |
| `Subscriber.hs:1431-1482` `LINK` handler | relay activation only after the creator's own async write |
| `Store/Groups.hs:2353-2373` `updateRelayGroupKeys` | throws for a chain owner without a row |
| `Subscriber.hs:4019-4020` | content from departed authors ignored |
| simplexmq `Server.hs:1574-1579`, `QueueStore/*` | `LSET` and `RKEY` are last-writer-wins and separate |
| simplexmq `Agent.hs:1337-1356` | the link read keeps the encrypted bytes local; no write without a local connection |

## 4. Design

The link server orders everything shared between owners. One new SMP command compares the hash of the stored encrypted user data and swaps the user data and, when owners change, the recipient key set in one atomic step. Every multi-owner link write reads, applies only its own change, and swaps. The link data commits the roster version, so each roster version has one author who had applied every earlier one: members and relays keep today's protocol unchanged.

### 4.1 simplexmq: server

- **Version.** `linkSwapSMPVersion = VersionSMP 24`; `currentClientSMPRelayVersion` and `currentServerSMPRelayVersion` 23 → 24 (`Transport.hs:228-232`); `proxiedSMPRelayVersion` stays 23 (recipient commands are not proxied). History lines in `Transport.hs:158-180` and `protocol/simplex-messaging.md:89-112`.
- **Command** (`Protocol.hs`): `LSWP :: LinkDataHash -> Maybe (NonEmpty RcvPublicAuthKey) -> EncUserDataBytes -> Command Recipient`, tag `LSWP_`/`"LSWP"`, encoded `e (LSWP_, ' ', h, keys_, ud)`, parsed `LSWP <$> _smpP <*> smpP <*> smpP`. `newtype LinkDataHash = LinkDataHash ByteString` (SHA-256, 32 bytes) of the stored `EncDataBytes` contents (nonce and ciphertext, without the wire length prefix). The fixed data and link ID are not sent: the server keeps them. An `LSET`-shaped command would leave room for about 6 keys in a 16,365-byte transmission; this one fits about 50 Ed25519 keys, so the link's user data bounds the owner count.
- **Error.** `CONFLICT` added to `ErrorType` (string and binary encodings, `Protocol.hs:1605-1703`, `:2145-2182`; JSON by TH). Only `LSWP` returns it; it is not delayed like `AUTH`.
- **Handler** (`Server.hs`, recipient commands near `:1574`): `LSWP h ks_ ud -> withQueue $ \q qr -> checkMode QMContact qr $ OK <$$ liftIO (swapQueueLinkData (queueStore ms) q h ks_ ud)`. Not in the proxy allow-list (`:2198-2211`), so a proxied `LSWP` gets `CMD PROHIBITED`.
- **Store method** in `QueueStoreClass` (`QueueStore/Types.hs:30-55`): `swapQueueLinkData :: s -> q -> LinkDataHash -> Maybe (NonEmpty RcvPublicAuthKey) -> EncUserDataBytes -> IO (Either ErrorType ())`. No link data → `AUTH`; hash differs → `CONFLICT`; else store `(fixed, ud)` under the same link ID and, when given, the key set.
  - STM (`QueueStore/STM.hs`): one `atomically` over `readQueueRec`, then the log line.
  - Postgres (`QueueStore/Postgres.hs`): one `withDB` transaction: `SELECT link_id, fixed_data, user_data FROM msg_queues WHERE recipient_id = ? AND deleted_at IS NULL FOR UPDATE`, compare in Haskell, `UPDATE msg_queues SET user_data = ? [, recipient_keys = ?] WHERE recipient_id = ?`; then write the cached record (real link bytes and keys) and log. The row lock orders concurrent swaps also in the Postgres-only store, whose queue lock is a no-op (`MsgStore/Postgres.hs:84`); the cache holds placeholder link bytes (`Postgres.hs:729`), so the compare cannot use it.
  - Journal store: delegate in `QStore` (`MsgStore/Journal.hs:304-361`).
- **Store log** (`Server/StoreLog.hs`): new record `CreateLinkKeys RecipientId LinkId QueueLinkData (NonEmpty RcvPublicAuthKey)`, tag `"LKEYS"` (the parser is an ordered `A.choice`, so the tag must not start with `LINK` or `KEYS`); replay (`StoreLog/ReadWrite.hs:40-79`) runs `addQueueLinkData` then `updateKeys`. A swap without keys logs the existing `CreateLink` with the full `QueueLinkData`. A restart therefore never restores data without its keys.
- **Client** (`Client.hs`): `swapSMPQueueLink :: SMPClient -> NetworkRequestMode -> RcvPrivateAuthKey -> RecipientId -> LinkDataHash -> Maybe (NonEmpty RcvPublicAuthKey) -> EncUserDataBytes -> ExceptT SMPClientError IO ()`, gated `thVersion (thParams c) >= linkSwapSMPVersion`, else `PCETransportError TEVersion` (pattern of `Client.hs:1080-1084`).

### 4.2 simplexmq: agent

Synchronous calls; the agent stores nothing new.

| Function | Does |
|---|---|
| `getConnShortLinkForWrite :: AgentClient -> NetworkRequestMode -> UserId -> ConnShortLink 'CMContact -> AE (FixedLinkData 'CMContact, ConnLinkData 'CMContact, LinkDataHash)` | the contact branch of `getConnShortLink'` (`Agent.hs:1337-1356`), proxied as `LGET` is, also returning the hash of the received encrypted user data |
| `setConnShortLinkSwap :: AgentClient -> NetworkRequestMode -> ConnId -> LinkDataHash -> Maybe (NonEmpty RcvPublicAuthKey) -> UserConnLinkData 'CMContact -> AE ()` | the creator's own link queue: validate owners, sign and encrypt as `prepareContactLinkData` does (`Agent.hs:1247-1268`, the user-data part shared), send `LSWP` with `rcvPrivateKey` |
| `setShortLinkSwapWithCreds :: AgentClient -> NetworkRequestMode -> UserId -> LinkOwnerCreds -> LinkDataHash -> Maybe (NonEmpty RcvPublicAuthKey) -> UserContactData -> AE ()` | any other owner: require its key in `owners` and `validateLinkOwners rootKey owners` (`Agent/Protocol.hs:2003-2017`), `encodeSignUserData` with its member key, `encryptUserData` with the key from `contactShortLinkKdf linkKey`, send `LSWP` signed with its member key |
| `deleteLinkQueueWithCreds :: AgentClient -> NetworkRequestMode -> UserId -> LinkOwnerCreds -> AE ()` | `DEL` with the owner's key, to destroy the channel |
| `getConnLinkQueue :: AgentClient -> ConnId -> AE (SMP.RecipientId, SMP.RcvPublicAuthKey)` | local read of the creator's link queue ID and public recipient key |
| `linkServerSupportsSwap :: AgentClient -> NetworkRequestMode -> UserId -> SMPServer -> SMP.RecipientId -> AE Bool` | connect on the owner's link session and compare the negotiated version with v24 |

- `LinkOwnerCreds {server :: SMPServer, linkKey :: LinkKey, rcvId :: SMP.RecipientId, rootKey :: C.PublicKeyEd25519, ownerKey :: C.PrivateKeyEd25519}`, assembled by chat from the channel link, the read's `linkRcvId`, the group's root public key and its member key.
- The creds calls use a transport session of their own per link, `(userId, server, Just rcvId)` through `withClient`/`withLogClient` (`Agent/Client.hs:1153-1159`): the default session is shared per user and server (`mkTSession`, `:1442-1444`) and would let the link server tie the owner's link writes to its other queues there.
- `Crypto/ShortLink.hs`: extract and export `mkOwnerAuth :: OwnerId -> C.PublicKeyEd25519 -> C.PrivateKeyEd25519 -> OwnerAuth`; `newOwnerAuth` (`:81-85`) uses it.
- Errors chat sees: `SMP _ CONFLICT`; `SMP _ AUTH` (the signer's key is not in the set); `BROKER _ (TRANSPORT TEVersion)` (server below v24); `CMD LARGE` (user data over 13,784 bytes).
- `DEL` with an unauthorised key already deletes the queue record locally (`deleteQueueRec`, `Agent.hs:3080-3088`). After an atomic leave or removal no later write restores the key, so the creator's normal link deletion is safe and no local-only delete is added.

### 4.3 Link data

`GroupShortLinkData` (`Protocol.hs:1621-1631`) gains optional fields; the agent-level `UserContactData` is unchanged.

| Field | Type | Written | Meaning |
|---|---|---|---|
| `linkVersion` | `Maybe Int64` | every channel link write: read value + 1 in a swap; stored value + 1 in the creator's single-owner writes (`groupLinkData`) | write counter; readers ignore a lower one (§4.4) and `XGrpInfo` carries it (§4.12) |
| `revokedOwners` | `Maybe [MemberId]` | removal and leave | departed owners; never shrinks |
| `creatorQueueKey` | `Maybe CreatorQueueKey {memberId :: MemberId, key :: SMP.RcvPublicAuthKey}` | the creator, in every swap from its first promotion | the creator's link-queue recipient key |
| `linkRcvId` | `Maybe SMP.RecipientId` | the creator, in every swap from its first promotion | queue address for other owners' commands; authorises nothing |
| `roster` | `Maybe LinkRoster {version :: VersionRoster, digest :: FD.FileDigest, author :: MemberId}` | every roster claim (§4.10) and the first promotion | the committed roster version, the SHA-512 of its blob, its author |

A write keeps JSON keys this version does not know: it encodes the record and merges it over the read JSON object (left-biased union). `GroupShortLinkData` today drops unknown fields on parse (`deriveJSON defaultJSON`).

**Multi-owner channel** (`isMultiOwnerChannel`): this device holds an owner row other than its own membership, current or departed; owner rows are never deleted in channels (§4.14). Every link write in a multi-owner channel goes through §4.4. Single-owner channels keep `setGroupLinkData`/`setGroupLinkDataAsync` (`Internal.hs:1588-1604`) and add only `linkVersion`.

### 4.4 Writes and reads

**`writeChannelLink :: User -> GroupInfoKeys -> ChannelLinkChange -> CM ChannelLink`**, called under the group lock:

1. Read with `getConnShortLinkForWrite` on `publicGroup.groupLink`; decode `UserContactData` and `GroupShortLinkData` with its raw JSON object.
2. Apply the read to local state (`applyChannelLinkData`, below). If it shows this device departed, stop with `CLENotOwner`.
3. Apply the change: `applyLinkChange :: ChannelLinkChange -> ChannelLink -> CM (Either ChannelLinkError (Maybe ChannelLink))`; `Nothing` means already applied: no write.
4. When the owner set changed, compute `linkQueueKeys`: `creatorQueueKey.key` unless the creator is revoked, plus `ownerKey` of every other chain entry not in `revokedOwners`. The last-owner check keeps it non-empty.
5. Set `linkVersion` to read (0 if absent) + 1; call `channelLinkWriteHook` (tests); swap: with `GRKPrivate` the creator uses `setConnShortLinkSwap` on its link connection; any other owner uses `setShortLinkSwapWithCreds`.
6. `CONFLICT` → repeat from 1, at most `channelLinkWriteAttempts` (5) times, then `CLELinkBusy`. `AUTH` → read and apply once more (it shows the revocation) and stop with `CLENotOwner`. `TEVersion` → `CLELinkServerVersion`. `CMD LARGE` → `CLELinkDataTooLarge`.
7. Store `groups.link_data_version`; return what was written.

There is no verifying re-read and no cache; the read is the token. Callers that run in message processing fork the write and take the group lock in the thread.

**Changes** (`ChannelLinkChange`):

| Constructor | Applies | Refuses |
|---|---|---|
| `CLCProfile old new` | the profile, as a whole | `CLEChangedByOwner` if the read profile ≠ `old`, after applying the read profile locally |
| `CLCAddOwner member ownerAuth` | entry appended after the signer's entry; key into the set; on the creator's first promotion also `creatorQueueKey`, `linkRcvId` (`getConnLinkQueue`) and `roster` (§4.6) | member in `revokedOwners`; already in the chain → no write (the commit still goes out) |
| `CLCRevokeOwner memberId` | memberId into `revokedOwners`; its key out | `CLELastOwner` when no other current owner remains; target already revoked → no write |
| `CLCRelays added removed` | read relays + added − removed − relays this device holds as left | — |
| `CLCCount n` | `publicGroupData.publicMemberCount = n` | no write unless this device is the first current entry in chain order and the read count ≠ `n` |
| `CLCRoster claim` | `roster` (§4.10) | §4.10 |

Chain rules: the chain stays in signing order (`validateLinkOwners`); the creator signs new entries with the root key (as `Commands.hs:2757`), any other owner with its member key via `mkOwnerAuth`; a revoked entry is dropped once no current entry depends on it, transitively, except the writer's own entry in its leave write (the agent accepts a non-root writer only while its key is in the chain, `Agent/Protocol.hs:1996-2001`). Each entry is about 124 bytes in the 13,784-byte user data.

**`applyChannelLinkData`** runs on every channel link read:

| Party | When |
|---|---|
| member | channel open (`APIGetUpdatedGroupLinkData`, `Commands.hs:1959-1972`), connect plan (`:4624`, `:4651`) |
| owner | reconcile (channel open and chat start) and step 2 of every write |
| relay | invitation (`getLinkDataCreateRelayLink`, `Subscriber.hs:4585-4606`) and every relay check (`checkRelayServedGroups`, `Commands.hs:6001-6020`) |
| joiner | `APIConnectPreparedGroup` (`Commands.hs:2303-2333`) |

Steps:

1. A read with `linkVersion` below `groups.link_data_version` is ignored whole; else store it.
2. Profile and count, as `updateGroupFromLinkData` does (`Internal.hs:1633-1654`).
3. `revokedOwners`: mark each held member departed (`GSMemLeft`), at whatever role it is held; create a departed row for an unknown memberId. Relays keep a departed owner's connection; they delete it on its `x.grp.leave` or a remover's `x.grp.mem.del`, because its last blob can be queued on it. This device's own memberId listed while its membership is current: complete its departure (§4.9 step 3).
4. Chain entries not in `revokedOwners`: a member held neither as owner nor departed becomes owner with `ownerKey` (created with a placeholder profile if unknown). A member held as removed: an owner runs "removal wins" (§4.8); others leave it removed. This device's own entry with its own key while its membership is current: become owner (§4.6); with another key: alert item, stay a subscriber.
5. `roster` → `link_roster_version`, `link_roster_digest`; `link_roster_seen_at` = now when the version changes.
6. Owners only: join each listed relay without a relay member (`connectToRelayAsync`, `Internal.hs:1606-1616`); set `RSInactive` on active `group_relays` rows missing from the list (as the `LINK` handler does, `Subscriber.hs:1466-1477`).

**Reconcile** = read and apply; when the read `roster.version` is above the owner's complete frontier (`applied_complete_roster_version`), `XGrpRosterRequest` to every current relay (an owner back after a long absence catches up here); at chat start, for each multi-owner channel where the user is a current owner, also the roster re-send and re-sign (§4.10) and a pending member catch-up (§4.7). It replaces the owner branch of `APIGetUpdatedGroupLinkData`.

### 4.5 Events and wire

`multiOwnerVersion = VersionChat 23`; `currentChatVersion` 22 → 23 (`Protocol.hs:96-97`).

| Event | Payload | Path | Signed | Forwarded |
|---|---|---|---|---|
| `x.grp.owner.inv` | `{memberId}` | owner → relays → the invitee's support scope | yes | yes, `DJSMemberSupport` of the invitee |
| `x.grp.owner.acpt` | `{invId :: SharedMsgId}` | invitee → relays → its support scope | yes | yes, `DJSMemberSupport` of the sender |
| `x.grp.owner.add` | `{memberId, ownerAuth :: OwnerAuth, profile :: Profile}` | owner → relays → all | yes | yes, `DJSGroup` |
| `x.grp.members.req` | `{cursor :: Maybe Int64}` | owner → one relay | no | no |
| `x.grp.members.page` | `{members :: [{memberInfo :: MemberInfo, memberRestrictions :: Maybe MemberRestrictions}], cursor :: Maybe Int64}` | relay → owner | no | no |
| `x.grp.info` (changed) | adds optional `linkVersion` | as today | yes | yes |

Each event: constructor, `CMEventTag`, tag string and parser, `toCMEventTag`, JSON params (`Protocol.hs:460-1557`), `requiresSignature` (`:1357-1371`), `isForwardedGroupMsg` (`:528-549`), relay dispatch (`Subscriber.hs:1080-1132`) and forwarded dispatch (`:4041-4064`). Older clients ignore them; older relays never see them (§5).

### 4.6 Promotion with consent

**Invite.** `APIInviteGroupOwner GroupId GroupMemberId` (CLI `/_owner invite #<groupId> <groupMemberId>`, `/owner invite #<group> <member>`) → `CROwnerInvitationSent {user, groupInfo, member}`. Under the group lock it refuses unless:

- the user is a current owner of a channel; M is a current member with a key, not an owner, not in `revokedOwners`;
- M's code is verified: `memberVerifiedCode` equals `getChannelMemberCode` now (`Commands.hs:3811-3815`), else `CLENotVerified`. A relay that substituted M's key would sign an acceptance with the substituted key, so only the out-of-band comparison detects it;
- every current relay supports `multiOwnerVersion` (`supportsVersion`), else `CLERelayVersion`;
- `linkServerSupportsSwap` on the link's server, else `CLELinkServerVersion`.

It sends `x.grp.owner.inv {memberId}` and stores the message's shared msg id in M's `group_members.owner_inv_msg_id` (a new invitation replaces it). M's version is not checked: it is known only from M's join (`Store/Groups.hs:2533-2551`); an older M ignores the invitation.

**Invitation on receivers.** Relay: sender a current owner and M a current member → forward to M's support scope. M: author a current owner and `memberId` its own → item `RGEOwnerInvitation {invStatus = OISPending}` in its own support scope (as `mkGetMessageChatScope` does for its own scope, `Internal.hs:1984-2003`). Other moderators: no item.

**Accept.** `APIAcceptGroupOwnerInvitation GroupId ChatItemId` on M: the item must be a pending invitation and M's membership current, not owner. It sends `x.grp.owner.acpt {invId}` (the item's shared msg id), sets the item to `OISAccepted`, returns `CRChatItemUpdated`. A relay forwards it to M's support scope; the promoter acts when `invId` equals M's `owner_inv_msg_id` and it is still a current owner, others ignore it.

**Promote.** The promoter forks, under the group lock:

1. Re-check M (current, not owner, not revoked) and the relays' versions.
2. Creator's first promotion only: wait (≤ `channelOwnerWaitTimeout`, 30 s) until its link connection has no pending `CFSetShortLink` command, else fail with `CLELinkBusy`: a queued single-owner `LSET` landing later would bypass the swap. Set `applied_complete_roster_version` to its `roster_version`, rebuild the roster blob from current roster members without M (`buildGroupRoster`), store it as its own (§4.10) and commit it in this write as `roster {its version, digest, itself}`.
3. `writeChannelLink` with `CLCAddOwner M (mkOwnerAuth …)`.
4. Clear `owner_inv_msg_id`; set M's role to owner locally (its key is already the chain key); send `x.grp.owner.add {memberId, ownerAuth, profile}` with M's redacted profile (`redactedMemberProfile`, `Internal.hs:1361`); first promotion: send the stored blob to current relays at that version (relays accept an equal version, `Subscriber.hs:3544-3545`); item `SGEMemberRole … GROwner`; `CEvtMemberRole` to the view.

A failure is shown to the promoter's user (`eToView`) and keeps `owner_inv_msg_id`; it is repeated by inviting again.

**Commit.** Relays (direct) and members and owners (forwarded) apply `x.grp.owner.add` only when: the sender is a current owner; `ownerAuth.ownerId == memberId`; `ownerAuth` verifies against the group root key or an owner key this device holds, current or departed (a delegated entry stays valid after its signer leaves); this device does not hold `memberId` as departed. Then: role owner, `member_pub_key = ownerKey` (warning when it replaces a different key), the member created from `profile` if unknown, the role-change item as `xGrpMemRole` creates it; a relay returns `DJSGroup` to forward. An owner that holds `memberId` as removed runs "removal wins" (§4.8). It is a new event, not `x.grp.mem.role … owner`, so older clients ignore it (fail closed) and no relay-asserted key is promoted (`Subscriber.hs:3465-3468`).

**M becomes owner** when it applies the commit about itself or reads its own entry (§4.4 step 4), with its own key and a current membership: role owner, item `RGEUserRole GROwner`, `members_catchup = 1`, start the member catch-up (§4.7). It stores no credential: each write builds `LinkOwnerCreds` from the channel link, the read's `linkRcvId`, `root_pub_key` and its member key. Before a commit for M arrives, other owners and members still treat M as a subscriber.

### 4.7 New owner's member list

- **Request.** The new owner sends `x.grp.members.req {cursor = Nothing}` to one current relay (the first connected relay member by `group_member_id`). Restarted at chat start while `groups.members_catchup = 1`, from the beginning.
- **Relay.** Serves only a current owner (relay row role owner, current). One page: current members (`getGroupMembersByCursor` statuses, `Store/Delivery.hs:352-390`) with `group_member_id > cursor`, excluding relays and the requester, in id order; `memberInfo` with the redacted profile and no image, plus restrictions; as many as fit one message (`maxEncodedMsgLength`, `Protocol.hs:929`); `cursor` = the last packed member's id, absent when no member follows.
- **Owner.** For a page from a current relay while `members_catchup = 1`: one transaction; members it holds are skipped; new ones are created with `xGrpMemIntro`'s channel rule (`Subscriber.hs:3326-3334`, extracted as a store-level function): a role from member up is created at the default role without a key (the roster establishes those), a subscriber keeps the relay-reported key, as owners keep it from `XGrpMemNew` today. No link write per member, unlike `xGrpMemNew` (`:3286`). Then request the next page, or set `members_catchup = 0` on the last.
- Without images about 40–60 members fit a message: about 2,000 messages for 100,000 members.

### 4.8 Removing an owner

- **Command.** `APIRemoveMembers` with a current owner as the only target, by a current owner, in a channel (owners can remove owners today, `Commands.hs:3131`; mixed targets refused). Under the group lock: `writeChannelLink` with `CLCRevokeOwner X`; then in one local step mark X removed (rows kept, §4.14) and send `x.grp.mem.del X False Nothing` through `deleteMemsSend` (`Commands.hs:3191-3229`). Removing oneself is a leave (§4.9).
- **Relays and members** apply `x.grp.mem.del` of an owner only from a current owner (`checkRole` already requires `senderRole >= memberRole`, `Subscriber.hs:3783-3786`); a relay forwards it to X first, then deletes X's connection (`:3755-3761`).
- **The removed owner's device** (the event's own-membership branch, `Subscriber.hs:3734-3746`, or its next read, §4.4 step 3) completes its departure: membership removed, keys erased as in §4.9 step 3, catch-up stopped. Its writes get `AUTH`.
- **Removal wins.** An owner that holds member M as removed and sees M as a current chain owner (a commit or a read) runs the owner removal of M: `CLCRevokeOwner M`; if the read already shows M revoked, it only marks M departed; otherwise it also sends `x.grp.mem.del M`. Concurrent attempts are ordered by the swap. Owners reject an admin's removal of a member they hold as owner (`checkRole`), so an admin cannot remove an owner through this.
- Mutual removal: the first swap removes the other's key; the second gets `AUTH`, or `CONFLICT` and then a read showing itself revoked.

### 4.9 Leaving

`APILeaveGroup` by an owner of a multi-owner channel (single-owner channels keep `Commands.hs:3233-3277`):

1. Without the group lock: read. Refuse with `CLELastOwner` if no other current owner. If `roster.author` is this owner and its stored roster version equals `roster.version`, re-send the stored blob to current relays (briefly under the lock) and wait up to `channelOwnerWaitTimeout` until `msg_deliveries` shows its last chunk `SENT` to at least one relay, else refuse with `CLERelaysUnreachable`. In channels a message to a relay that is not ready is dropped, not queued (`memberSendAction`, `Internal.hs:2840-2846`), and leaving waits only for messages already in the agent (`deleteMembersConnections' … True`). The wait is outside the lock because `SENT` is processed under it (`Subscriber.hs:146-147`).
2. Under the lock: `writeChannelLink` with `CLCRevokeOwner self`, keeping its own entry and dropping its key; the last-owner check runs on every attempt, so the last two owners leaving at once leave one.
3. Complete the departure: send `x.grp.leave` to relays; delete member connections waiting for delivery; membership `GSMemLeft`; erase `member_priv_key` (a later load generates an unrelated key, `Store/Shared.hs:750-762`); the creator replaces `root_priv_key` with the root public key and deletes its group link through `deleteGroupLinkIfExists`, whose `DEL` now gets `AUTH` and deletes locally.

A crash before step 2 leaves the owner current. After it, the next read (chat start reconcile) finds its own memberId revoked and runs step 3.

### 4.10 Roster ordered by the link

**Claim.** In a multi-owner channel, `APIMembersRole` and `APIRemoveMembers` call `claimRosterVersion user g delta targets` instead of `broadcastRoster` (`Commands.hs:3005-3008`, `:3136-3139`). It runs `writeChannelLink` with `CLCRoster`:

1. With `r` = read `roster.version` and `f` = `applied_complete_roster_version`: if `r > f`, send `XGrpRosterRequest (Just f)` to every current relay and refuse with `CLERosterSyncing`.
2. If any target's role differs from the role loaded when the command started, refuse with `CLEChangedByOwner` (no silent override).
3. Build the blob at `r + 1` from current roster members with the delta applied (`buildGroupRoster (applyRosterDelta delta …)`, `Internal.hs:2629-2654`); `roster = {r + 1, SHA-512 of the blob, self}`.
4. After the swap: `setOwnGroupRoster` sets `roster_version`, `applied_complete_roster_version`, `stored_roster_version` to `r + 1`, `roster_blob`, and the sending owner to its membership (header columns stay NULL); send the blob (`sendRoster`); return `r + 1` for the events.

A role change in a single-owner channel keeps `broadcastRoster`.

**Receiving.** Owners apply other owners' role events through `applyAtRosterVersion` and request blobs on a gap, as members do (`Subscriber.hs:3404-3446`). Members and relays are unchanged: each version has one author who had applied every earlier one, so the gate, completion and gap rules suffice. Owners stay outside the roster: `processRosterEntries` skips current owners and departed members (`:3610-3630`); `xGrpMemRole` ignores an owner target and any event assigning the owner role; `xGrpMemDel` of an owner is the removal of §4.8.

**Departed authors.** A blob whose author has departed is accepted only if its header's version and digest equal `link_roster_version` and `link_roster_digest`:

- `xGrpMsgForward` (`Subscriber.hs:4019-4020`): an `XGrpRoster` from a departed owner is passed on to `xGrpRoster` on an owner device; other content stays ignored.
- Relay direct path: from a departed member's connection only `XGrpRoster` and `XGrpLeave` are processed; anything else is dropped with a log (today only role is checked, so `x.grp.del` from a departed owner would delete the channel on the relay).
- `xGrpRoster` (`:3503-3540`) checks the commitment for a departed author before starting the transfer; `rosterCompletion` is unchanged.

**Re-sign** (`resignRosterIfNeeded`), run when an owner applies another owner's departure (`x.grp.leave`, `x.grp.mem.del` of an owner, `revokedOwners` in a read), completes a departed author's blob, and in reconcile at chat start. It is a `CLCRoster` write, so it decides on its own read: it acts when the read `roster.author` has departed, or is this owner with a stored version below the read's (its claim was lost before it was stored); otherwise no write:

- if `f >= r`: claim `r + 1` with the current roster;
- else if the author is this owner, or `link_roster_seen_at` is older than `rosterAbandonTTL` (1 day): claim `r + 1` from its own state, abandoning `r` (nobody received it);
- else: send `XGrpRosterRequest (Just f)` to every current relay and wait.

After a re-sign claim it sends the blob and one `x.grp.mem.role` at `r + 1`: the current role of the first roster member by memberId (no event when there are none). Members that dropped the departed author's events see a gap and fetch the blob. The swap lets only one owner claim `r + 1`; others' reads then show a current author.

**Chat start.** An owner that is the link's roster author with an equal stored version re-sends its stored blob (a repeat is harmless; a relay accepts an equal version).

**Relay acknowledgement below the current version** (`xGrpRosterAck`, `Subscriber.hs:3654-3672`): re-send the current roster to that relay (`sendGroupRosterToRelay`) instead of only warning, since another owner can advance the version during relay set-up.

### 4.11 Subscriber count

In a multi-owner channel the owner branch of `updatePublicGroupData` (`Internal.hs:1618-1631`) recounts locally (`updatePublicMemberCount`) and schedules a publish when `members_catchup = 0`. Scheduling: `ChatController.countPublishPending :: TMap GroupId ()`; a trigger that finds no entry inserts one and forks a thread that sleeps `channelCountPublishInterval` (60 s), runs `writeChannelLink` with `CLCCount tally` under the group lock and deletes the entry. Only the first current owner in the read chain writes, so other owners make at most one proxied read a minute while the count changes; one owner's tally is complete once its member catch-up is done. The relay-announcement threshold in relay activation becomes `publicMemberCount > number of current owners` (`Subscriber.hs:1445-1449`). Relays and single-owner channels are unchanged.

### 4.12 Profile edits and the link counter

`runUpdateGroupProfile` (`Commands.hs:4164-4192`) in a multi-owner channel calls `writeChannelLink` with `CLCProfile old new`, where `old` is the profile when the command started; then sends `XGrpInfo` with the written `linkVersion`. Single-owner writes also carry `linkVersion` (§4.3). `xGrpInfo` (`Subscriber.hs:3840-3865`) ignores an event whose `linkVersion` is below `link_data_version` (a relay returns no delivery scope for it) and stores a higher one.

### 4.13 Relays in a multi-owner channel

- **Activation.** The `LINK` handler body (`Subscriber.hs:1437-1477`) becomes `activateGroupRelays :: User -> GroupInfoKeys -> [ShortLinkContact] -> CM ()`, called by the handler (single-owner async writes) and after a `CLCRelays` swap. It reads `group_relays` and the group link row only if present; `CEvtGroupLinkDataUpdated` and `CRGroupRelaysAdded` carry `Maybe GroupLink` (`Controller.hs:862`, `:1035`).
- **Publication** after the relay's acknowledgement (`Subscriber.hs:3661-3665`) and on a pre-roster relay's connection (`:946-947`): a forked `CLCRelays [link] []` instead of `setGroupLinkDataAsync`.
- **Removal** of a relay (`Commands.hs:3149-3153`) and a relay's own leave processed by any owner (`xGrpLeave` → `updatePublicGroupData`): `CLCRelays [] [link]`, a no-op when absent.
- **`APIAddGroupRelays`** by an owner without a group link row takes the link from `publicGroup.groupLink` (`Commands.hs:2790-2793`).
- **An owner joins relays added by other owners** from `XGrpRelayNew` (`Subscriber.hs:3881-3886`) and in reconcile. Relay side: `memberJoinRequestViaRelay` (`:1690-1720`) accepts a key-proven join onto a current owner row, as for roster rows, and refuses joins onto departed rows. Owner side: `updatePreparedUserAndHostMembers'` (`Store/Groups.hs:821-844`) and the host `CON` branch (`Subscriber.hs:892-922`) leave the membership role, status and group profile and the joined items alone when the membership is an owner.
- **Relay invitation by any owner.** `updateRelayGroupKeys` (`Store/Groups.hs:2353-2373`) creates an owner row (`createLinkOwnerMember`) for every chain owner without one and applies `revokedOwners`; the inviting-owner check (`Subscriber.hs:4598`) excludes `revokedOwners`.
- **Relay capabilities.** `XGrpRelayCap` (`Subscriber.hs:1103-1105`) from a relay this owner has no `group_relays` row for is stored on the relay's member row, `group_members.relay_web_domain` (`relayWebDomain` in `GroupMember` JSON); today `updateRelayCapabilities` (`Store/Groups.hs:1901-1911`) updates only `group_relays`. An owner's channel web settings take relay domains from its rows and from relay members.

### 4.14 Other guards

- **Durable rows.** In channels, `getGroupMembersForExpiration` and `getRemovedMembersToCleanup` (`Store/Groups.hs:1357-1382`) skip rows with the owner role or a roster role; an intro (`xGrpMemIntro`, `Subscriber.hs:3320-3322`) never changes a departed member's status.
- **`xGrpDel`** (`Subscriber.hs:3830`) requires a current owner.
- **Group-link commands** `APICreateGroupLink`, `APIAddGroupShortLink`, `APIDeleteGroupLink` are refused for channels (`Commands.hs:3349-3387`).
- **`APIShareChatMsgContent`** signs with the member key of any current owner (`Commands.hs:1238-1240`).
- **Connect plan and join** exclude `revokedOwners` from `glOwners`, `verifyLinkOwner` and owner binding (`Commands.hs:4615-4653`, `:4806-4815`); `APIConnectPreparedGroup` creates owners only for non-revoked entries and departed rows for revoked ones (`:2324-2332`).
- **History for joiners.** A signed as-channel item whose author is no longer a current owner is served as an unsigned channel item (`itemForwardMsgs`, `Internal.hs:1540-1548`).
- **Destroying.** A promoted owner's `APIDeleteChat` in the branch that sends `x.grp.del` (`Commands.hs:1394-1401`) also deletes the link queue with `deleteLinkQueueWithCreds`. The creator keeps `deleteGroupLinkIfExists`: current, its `DEL` deletes the queue; departed, it gets `AUTH` and deletes locally.

### 4.15 Types, database, configuration

| Kind | Name | Notes |
|---|---|---|
| command | `APIInviteGroupOwner GroupId GroupMemberId` | → `CROwnerInvitationSent` |
| command | `APIAcceptGroupOwnerInvitation GroupId ChatItemId` | → `CRChatItemUpdated` |
| error | `CEChannelLink {channelLinkError :: ChannelLinkError}` | `CLENotVerified`, `CLERelayVersion`, `CLELinkServerVersion`, `CLEChangedByOwner`, `CLERosterSyncing`, `CLELastOwner`, `CLENotOwner`, `CLELinkDataTooLarge`, `CLELinkBusy`, `CLERelaysUnreachable` |
| item | `RGEOwnerInvitation {invStatus :: OwnerInvitationStatus}` | `OISPending`, `OISAccepted` |
| column | `groups.link_data_version INTEGER` | highest `linkVersion` read or written |
| column | `groups.link_roster_version INTEGER`, `link_roster_digest BLOB`, `link_roster_seen_at TEXT` (`TIMESTAMPTZ` in Postgres) | the last read roster commitment; when its version first appeared |
| column | `groups.members_catchup INTEGER NOT NULL DEFAULT 0` | new owner's catch-up in progress |
| column | `group_members.owner_inv_msg_id BLOB` | on the promoter: the pending invitation to this member |
| column | `group_members.relay_web_domain TEXT` | web domain a relay advertised to an owner without its `group_relays` row |
| config | `channelLinkWriteAttempts = 5`, `channelCountPublishInterval = 60 s`, `rosterAbandonTTL = 1 day`, `channelOwnerWaitTimeout = 30 s` | `ChatConfig` |
| hook | `channelLinkWriteHook :: Maybe (GroupInfo -> IO ())` | `ChatHooks`, between read and swap; tests only |
| state | `countPublishPending :: TMap GroupId ()` | `ChatController` |

One migration `M20261015_channel_multi_owner` for SQLite and Postgres (register in both `Migrations.hs` and `simplex-chat.cabal`; schema dumps regenerate from tests). Existing roster columns are reused.

### 4.16 Races and where each is ordered

| Race | Ordered by | Where | Outcome |
|---|---|---|---|
| Two link writes of any kind | the link server's swap | `swapQueueLinkData` | the second re-reads and re-applies, or is refused (`CLEChangedByOwner`) |
| A promotion by O1 while O2 removes O1 | the swap (keys and data in one write) | `writeChannelLink` | removal first: O1 gets `AUTH` or `CONFLICT` then reads itself revoked; else the removal re-reads and keeps M's key |
| Two owners remove each other | the swap | §4.8 | one remains |
| A removal and a leave; the last two owners leaving | the swap; last-owner check per attempt | §4.4 change table | one current owner remains |
| Two role changes | `roster.version` in the link | `claimRosterVersion` | the second catches up and claims `v + 2`, or is refused for the same target |
| A role change just before leaving | the commitment, plus the leaver's wait for `SENT` | §4.9 step 1, §4.10 | the committed blob is accepted from the departed author; the re-sign reaches members |
| Two profile edits | the swap; `linkVersion` on members | §4.12 | the second is refused if the profile changed; members keep the newer |
| A relay added while another is removed | the swap | `CLCRelays` | both changes kept, no tombstone |
| A member removed while it is promoted | "removal wins" | §4.8 | an owner that saw the removal revokes M |
| A party that was offline | link data applied on every read | §4.4 | converges on its next read |
| A single-owner write queued before the first promotion | wait for pending async writes | §4.6 step 2 | it lands before the first swap |

## 5. Compatibility and security

- **Wire.** SMP v24 (`LSWP`, `CONFLICT`), negotiated as today (`compatibleVRange'`); a v23 server gets no `LSWP` (client gate). Chat v23 events (§4.5); `XGrpInfo` gains an optional field.
- **Database.** One migration, columns only, both backends; existing rows get NULL or 0. No agent or server migration (`sha256` is computed in Haskell).
- **Server downgrade.** An older server skips `LKEYS` lines (logged) and compacts, losing swaps that changed keys since its last start, as with every store-log record added before.
- **Older parties.** Promotion needs every current relay at chat v23 and the link server at SMP v24. Older members ignore `x.grp.owner.add` and keep rejecting the new owner's signed messages until they upgrade (they see its unsigned posts as channel posts); older joiners take every chain entry as an owner, including revoked ones, whose keys no longer authorise anything on the link queue or at relays.
- **Adversary.**
  - Relay: cannot forge owner-signed events, commits or acceptances; can withhold or reorder, as today; cannot substitute a promoted member's key (verified code); can fabricate subscribers in a member page (as with `XGrpMemNew` today), never privileged ones.
  - Link server: cannot forge link data (signatures, `validateLinkOwners`); a stale read is ignored by `linkVersion`; ignoring the compare-and-swap brings back lost writes and roster divergence between honest owners, but no forgery.
  - Removed owner: its key leaves the set in the removing write; a copy of its member key (or the creator's root key) can sign blobs that owners and relays accept only when committed in the link, and with a colluding link server it can serve forged link data to readers (residual).
  - Any current owner can destroy the channel or replace the key set with `RKEY`: owners are trusted as today's single owner.
  - Resource use: one page per request, paced by the owner; owners only.
- **Privacy.** A promoted owner contacts the link server directly for its user's actions and, once it is the first current owner, for count publishing; it accepted both with the invitation. Its link commands use a session of their own. Link reads stay proxied. The link server learns the timing of role changes. `linkRcvId` and `creatorQueueKey` are readable by anyone with the link and authorise nothing.

## 6. Slices

Each slice compiles and passes its tests on its own. Verify commands: simplexmq `cabal test simplexmq-test --test-options='-m "<pattern>"'` (Postgres tests with `-fserver_postgres`, on the host); chat `cabal test simplex-chat-test --test-options='-m "channels"'`. simplexmq slices land on a simplexmq branch from `cba13823`; chat slices use `use-local-simplexmq` until the simplexmq commit is pinned.

| # | Slice | Kind | Files | Done when |
|---|---|---|---|---|
| S1 | SMP v24, `LSWP`, `CONFLICT`, client call, protocol doc | behavioural | `Transport.hs`, `Protocol.hs`, `Server.hs`, `Client.hs`, `protocol/simplex-messaging.md` | server tests below pass for the STM store; a transmission with 50 Ed25519 keys fits |
| S2 | `swapQueueLinkData` in STM, Postgres, journal; `CreateLinkKeys` record and replay | behavioural | `QueueStore/{Types,STM,Postgres}.hs`, `MsgStore/Journal.hs`, `StoreLog.hs`, `StoreLog/ReadWrite.hs` | store and restart tests pass for every store |
| S3 | agent: read with hash, two swaps, `DEL` with creds, queue read, version check, own session, `mkOwnerAuth` | behavioural | `Agent.hs`, `Agent/Client.hs`, `Crypto/ShortLink.hs` | agent tests pass |
| S4 | chat structure: extract `activateGroupRelays` from the `LINK` handler, the channel intro member creation from `xGrpMemIntro`, `mkOwnerAuth` use | structural | `Subscriber.hs`, `Store/Groups.hs`, `Internal.hs` | `-m "channels"` green, no behaviour change |
| S5 | chat data and wire: migration, link-data fields, events, versions, errors, commands parsed (refusing), config, hook | behavioural (inert) | `Protocol.hs`, `Types.hs`, `Controller.hs`, migrations, `simplex-chat.cabal` | `-m "channels"`, protocol and JSON tests green |
| S6 | writes and reads: `writeChannelLink`, `applyChannelLinkData`, reconcile, `linkVersion` on every channel write and read, `XGrpInfo` gating, durable rows, intro guard, relay check and invitation apply | behavioural | `Internal.hs`, `Commands.hs`, `Subscriber.hs`, `Store/Groups.hs` | T12 and existing tests |
| S7 | promotion with consent, commit, M becomes owner, member catch-up | behavioural | as S6 | T1, T2, T10 |
| S8 | removal, removal wins, leave, removed device, `x.grp.del` and departed-member guards, group-link commands, sharing, connect plan, history, destroy by a promoted owner | behavioural | as S6 | T3, T4, T6–T8 |
| S9 | roster claim, owners outside the roster, departed-author acceptance, re-sign, abandonment, chat-start re-send, leave wait, acknowledgement re-send | behavioural | `Internal.hs`, `Subscriber.hs`, `Commands.hs`, `Store/Groups.hs` | T5, T9 |
| S10 | relays by any owner, count publisher, threshold, profile refusal | behavioural | as S6 | T5 (profile, relays), T11 |
| S11 | iOS | behavioural | §6.1 | host build; manual checks §7 |
| S12 | Kotlin, mirroring S11 | behavioural | §6.1 | host build; manual checks §7 |

### 6.1 Apps (iOS and Kotlin, mirrored)

Same file split, names and branch order on both; the Kotlin file is named after the Swift one.

| Change | iOS | Kotlin |
|---|---|---|
| API: `apiInviteGroupOwner`, `apiAcceptGroupOwnerInvitation`; `ownerInvitationSent`; `groupLink` optional in `groupRelaysAdded`, `groupLinkDataUpdated`, `AddGroupRelaysResult.added` | `SimpleXAPI.swift`, `AppAPITypes.swift:1001, 1210` | `SimpleXAPI.kt:6815, 6846` |
| Errors: `channelLink(ChannelLinkError)` with one alert per case; SMP `CONFLICT` | `APITypes.swift:744+` | `SimpleXAPI.kt:7807+` |
| `RcvGroupEvent.ownerInvitation(invStatus)`: text, notification, bold like `newMemberPendingReview`; tapping a pending one opens "Become an owner?" (it states that the device will contact the link server for channel changes and the count, and that the member's profile goes to all subscribers) with Accept | `ChatTypes.swift:6054+`, `ChatItemView.swift:153-157` | `ChatModel.kt:5447+`, `ChatItemView.kt:734-748` |
| Role picker: channels offer Owner to a current owner for a current non-owner member with a key; picking it opens verification if unverified (`APIVerifyGroupMember`), else confirms the invitation (the member is asked, owners can remove owners, any owner can delete the channel) and calls `apiInviteGroupOwner`; owner targets show no picker | `ChatTypes.swift:3382-3394`, `GroupMemberInfoView.swift:327-331, 731-768` | `ChatModel.kt:2944-2955`, `GroupMemberInfoView.kt:149-158, 947-991` (roles list keyed so it recomputes) |
| Remove: "Remove owner" label and confirmation for owner targets | `GroupMemberInfoView.swift:697-707`, `GroupChatInfoView.swift:995-1044` | `GroupMemberInfoView.kt:843-854`, `:253-334` |
| Leave: offered to an owner when another current owner exists, in the chat list and info view; core refusals shown | `ChatListNavLink.swift:247, 272`, `GroupChatInfoView.swift:249-254`, `SimpleXAPI.swift:2047-2060` | `ChatListNavLinkView.kt:322, 344`, `GroupChatInfoView.kt:828-838`, `SimpleXAPI.kt:2493-2498` |
| Link screen: an owner without a `GroupLink` sees the published link; never creates a link for a channel | `GroupChatInfoView.swift:113-127, 402-403`, `GroupLinkView.swift:164-166` | `GroupChatInfoView.kt:599-611, 642-653`, `GroupLinkView.kt:119-123` |
| Channel open: owners also call `apiGetUpdatedGroupLinkData` (reconcile) | `ChatView.swift:762-780` | `ChatView.kt:225-242` |
| Composer: an owner without `group_relays` rows uses the relay-member state | `ComposeView.swift:739-773` | `ComposeView.kt:2151-2177` |
| Web settings: relay domains from `group_relays` rows plus relay members' `relayWebDomain` | `ChannelWebAccessView.swift:86-120` | `ChannelWebPageView.kt` (its relay-domain list) |
| Strings | `en.xliff` via `NSLocalizedString` | `moko-resources/base/strings.xml` |

Not compiled in the container; the user builds both on the host.

## 7. Test plan

**simplexmq** (`tests/ServerTests.hs` for each store; `tests/CoreTests/StoreLogTests.hs`; `tests/AgentTests/FunctionalAPITests.hs`):

- `LSWP` with the right hash replaces the user data and keeps the fixed data; a wrong hash gets `CONFLICT` and changes nothing; no link data gets `AUTH`; a messaging queue gets `AUTH`; via a proxy `CMD PROHIBITED`; a v23 client session cannot send it.
- `LSWP` with keys: the removed key's next command gets `AUTH`, the added key writes; concurrent swaps from two keys: one `OK`, one `CONFLICT`, then the second succeeds on the new hash (Postgres-only store included).
- Restart: a key-changing swap replays as data and keys; a data-only swap replays as `LINK`.
- Size: a transmission with the maximum user data and 50 Ed25519 keys encodes within the block.
- Agent: `getConnShortLinkForWrite`'s hash is accepted by both swaps; a non-creator owner's swap is readable and verified by `getConnShortLink`; a swap after the signer's key was removed fails with `AUTH`; `deleteLinkQueueWithCreds` deletes the queue; `linkServerSupportsSwap` is false on a v23 test server.

**chat**, a new `describe "multiple owners"` under `describe "channels"` (`tests/ChatTests/Groups.hs:281`), relays as in `createChannel2Relays` (`:9616`):

| # | Test |
|---|---|
| T1 | promotion: verify code, invite, accept; a subscriber that joined earlier accepts M's signed post; a member that knew M through a relay-asserted key ends with the chain key; M edits the profile and changes a role |
| T2 | refusals: unverified; a relay below v23 (relay with an older `chatVRange`); a link server at SMP v23; M's device ignores a chain entry with a key that is not its own |
| T3 | hand-over: creator promotes M and leaves; M posts through every relay, edits the profile (existing and new subscribers see it), adds a relay (active, other relays get `XGrpRelayNew`), removes a relay, promotes a verified subscriber, destroys the channel (link no longer resolves); a later joiner sees M as owner, the creator departed, verifies the roster, gets the creator's history unsigned; the creator's leave sent no `DEL` that succeeded and the link resolved until the destroy |
| T4 | removal: O1 removes O2; O2's device shows it removed, its stored member key no longer matches its chain entry, its next write fails; members and a new joiner treat O2 as departed |
| T5 | concurrency with `channelLinkWriteHook` holding O1 between read and swap: O2's profile edit lands, O1's edit is refused with `CLEChangedByOwner`; O1's relay addition and O2's relay removal both end in the link; O1 and O2 change roles of different members: every relay serves a blob with both, every member applies both, a joiner gets both |
| T6 | mutual removal with the hook: one owner remains |
| T7 | last two owners leave with the hook: one leave refused |
| T8 | removal wins: O2 removes subscriber M while O1's promotion of M is held by the hook; M ends departed everywhere |
| T9 | role change before leaving: O1 demotes an admin and leaves while O2 is offline; O2 returns, reads the link, requests and accepts O1's committed blob, re-signs; members that dropped O1's events fetch on the gap; a joiner gets the re-signed roster; the demotion holds |
| T10 | member catch-up: subscribers joined before the promotion appear on M with keys (it verifies and promotes one); a restart mid-catch-up restarts and skips held members |
| T11 | count: after the creator leaves, joins and leaves change the published count within the interval (shortened in the test config) |
| T12 | `linkVersion`: an `XGrpInfo` with a lower version is ignored; a stale read does not revert the profile |

Each acceptance criterion maps to these (§1). Existing tests that must stay green: the whole `-m "channels"` group (single-owner paths), `ProtocolTests`, `JSONTests`, schema dump. Full suite for the final check: `cabal test simplex-chat-test` and `simplexmq-test` (Postgres variants on the host).

**Manual app checks** (one layout; nothing here depends on layout): invite and accept on two devices, the invitation alert, Owner option and verification routing, Remove owner, leave refusal as last owner, promoted owner's link screen and composer, relays screen.

## 8. Risks

- **Riskiest assumption:** that members' and relays' unchanged roster protocol converges when versions are claimed through the link. Tested by T5 and T9 in S9; S1 first proves the swap's atomicity and size, on which the claim rests.
- The departed-author exception touches the drop rule that protects members; it is limited to roster blobs, on owners and relays, matching a committed digest.
- Postgres server tests run only on the host.
- App code is not compiled in the container.

## 9. Open questions

1. The link server must be reachable for every role change in a multi-owner channel (about 1–3 s). Decided by the user: accepted.
2. Consent and key-substitution defence: verified code plus an invitation in the channel. Decided by the user.
3. Member list: pages pulled from one relay, no images. Decided by the user.
4. `LSWP` sends only the user data (decisions §4 of the run). Recommended: as planned. If not, owners are capped at about 6.

## 10. Out of scope

Pre-existing single-owner issues: relay forwarding order (`Store/Delivery.hs:178-224`); messages to a backlogged relay dropped (`Internal.hs:2840-2846`); a queued async link write overwriting a newer one in single-owner channels (`Agent.hs:1191-1200`); moderator and admin promotion using relay-reported keys (`Commands.hs:3053`); block and unblock not ordered.

## 11. Follow-up runs

None: the whole feature is planned here.
