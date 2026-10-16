# Channel multi-owner (S3-min) — implementation plan

Branch: `f/channel-multi-owner` · Repos: simplexmq (from pinned `cba13823`), simplex-chat · Estimated diff: ~2,300 production Haskell lines (simplexmq ~470, chat ~1,810) plus tests, and ~400 lines per app platform.

The overview, `2026-10-15-channel-multi-owner-s3min.md`, says what owners can do and why; this file says what to build. Anchors are simplex-chat paths under `src/Simplex/Chat/` and simplexmq paths under `src/Simplex/Messaging/` at `cba13823`.

## 1. Goal and acceptance criteria

A channel has several equal owners. Any owner can do what the single owner does today; owners are added with the member's consent, can remove each other and can leave; concurrent owner actions are ordered by the link server, so none is lost, overwritten unseen or reverted.

| # | Criterion | Tests (§7) |
|---|---|---|
| AC1 | An owner invites a member whose code it verified; the member accepts; one link write adds the member's chain entry and recipient key; every relay and subscriber treats it as owner; it posts, edits and changes roles | T1, T20, T21 |
| AC2 | Promotion is refused for an unverified member, while a relay is below `multiOwnerVersion`, and on a link server below SMP v24 | T2 |
| AC3 | The creator promotes M and leaves; M does every owner action; a later joiner sees M as owner and the creator as departed, verifies the roster and gets the creator's history unsigned | T3 |
| AC4 | An owner removes another; the removed owner's link writes and `DEL` get `AUTH`; its device shows the removal and erases its keys; relays refuse its owner additions, owner removals and channel deletion | T4, T17 |
| AC5 | Concurrent link writes both land or one is refused; a profile edit over a profile changed since the editor opened is refused; concurrent role changes reach every relay and member and no demotion is reverted; mutual removal and the last two owners leaving each leave one current owner | T5–T8, T14 |
| AC6 | A role change made just before its author leaves reaches every device | T9, T15, T22 |
| AC7 | A new owner pulls the member list from a relay; the count keeps updating after the creator leaves | T10, T11 |
| AC8 | An older profile event or link read never replaces a newer profile | T12 |
| AC9 | `LSWP` swaps link data and keys atomically on every store, and a restart replays both | §7 simplexmq |
| AC10 | Single-owner channels behave as today: the existing `-m "channels"` group passes | §7 |

## 2. Non-goals

Retiring or rotating the root key; protection against a malicious current owner; ordering block and unblock; relay forwarding order and queueing to backlogged relays; field-level profile merge; equal-version roster tie-break; decline, withdrawal or cancel of owner invitations; profile images in the member catch-up; older apps beyond failing closed.

## 3. Current state

| Where | Today |
|---|---|
| `Internal.hs:1669-1685` `groupLinkData` | owners = `[self]`, only with the root private key |
| `Internal.hs:1618-1631` `updatePublicGroupData` | creator republishes the count on every join or leave; throws without a group link row |
| `Commands.hs:2994-2995` | owner role refused in channels |
| `Commands.hs:3246`, `Internal.hs:1739-1748` | leaving deletes the link queue with `DEL` |
| `Internal.hs:2642-2654` `broadcastRoster` | roster version = local gate + 1, one signer |
| `Subscriber.hs:1431-1482` `LINK` handler | relays activated only after the creator's own async write |
| `Store/Groups.hs:2353-2373` `updateRelayGroupKeys` | throws for a chain owner without a row |
| `Subscriber.hs:4019-4020` | forwarded content from departed authors ignored |
| simplexmq `Server.hs:1574-1579`, `QueueStore/*` | `LSET` and `RKEY` are separate and last-writer-wins |
| simplexmq `Agent.hs:1337-1356` | the link read keeps the encrypted bytes local; no write without a local connection |

## 4. Design

The link server orders everything owners share. One new SMP command compares the hash of the stored encrypted user data and swaps the user data and, when owners change, the recipient key set in one atomic step. Every multi-owner link write reads, applies only its own change, and swaps. The link data commits the roster version, so each version has one author who had applied every earlier one: members and relays keep today's roster protocol.

### 4.1 simplexmq: server

- **Version.** `linkSwapSMPVersion = VersionSMP 24`; `currentClientSMPRelayVersion` and `currentServerSMPRelayVersion` 23 → 24 (`Transport.hs:228-232`); `proxiedSMPRelayVersion` stays 23. History lines in `Transport.hs:158-180` and `protocol/simplex-messaging.md:89-112`.
- **Types and error** (`Protocol.hs`): `newtype LinkDataHash = LinkDataHash ByteString`, the SHA-256 (32 bytes) of the stored `EncDataBytes` contents (nonce and ciphertext, no wire length prefix). `CONFLICT` added to `ErrorType` (string and binary encodings `:1605-1703`, `:2145-2182`; JSON by TH); only `LSWP` returns it and it is not delayed like `AUTH`.
- **Command:** `LSWP :: LinkDataHash -> Maybe (NonEmpty RcvPublicAuthKey) -> EncUserDataBytes -> Command Recipient`, tag `"LSWP"`, `e (LSWP_, ' ', h, keys_, ud)`, parsed `LSWP <$> _smpP <*> smpP <*> smpP`. The fixed data and link ID are not sent; the server keeps them. A transmission (16,365 bytes, `Protocol.hs:2359-2360`) carries the 13,826-byte user data and up to 52 Ed25519 keys; an `LSET`-shaped command would fit 6.
- **Handler** (`Server.hs`, near `:1574`): `LSWP h ks_ ud -> withQueue $ \q qr -> checkMode QMContact qr $ OK <$$ liftIO (swapQueueLinkData (queueStore ms) q h ks_ ud)`. Not in the proxy allow-list (`:2198-2211`).
- **`LSET` on a queue with more than one recipient key gets `AUTH`** (`Server.hs:1575-1579`). Only multi-owner links have several keys (nothing sends `RKEY` today), so a stale single-owner write, or a creator's app downgraded to a version without `LSWP`, cannot overwrite multi-owner link data.
- **Store method** in `QueueStoreClass` (`QueueStore/Types.hs:30-55`): `swapQueueLinkData :: s -> q -> LinkDataHash -> Maybe (NonEmpty RcvPublicAuthKey) -> EncUserDataBytes -> IO (Either ErrorType ())`. No link data → `AUTH`; hash differs → `CONFLICT`; else store `(fixed, ud)` under the same link ID and, when given, the key set. The swap, the cached record and the log lines run under `withQueueRec` (`Postgres.hs:760-762`; the queue lock, real in the journal store).
  - STM (`QueueStore/STM.hs`): one `atomically` over `readQueueRec`, then the log. The STM message store's queue lock is a no-op (`MsgStore/STM.hs:64`); its log order rests on the next swap needing this swap's hash, which takes the next writer a network round trip to learn.
  - Postgres (`QueueStore/Postgres.hs`): one `withDB` transaction: `SELECT link_id, fixed_data, user_data FROM msg_queues WHERE recipient_id = ? AND deleted_at IS NULL FOR UPDATE`, compare in Haskell, `UPDATE msg_queues SET user_data = ? [, recipient_keys = ?] WHERE recipient_id = ?`; then the cached record gets the real bytes and keys. The row lock orders swaps in the Postgres-only store, whose queue lock is a no-op and which has no cache (`MsgStore/Postgres.hs:84`, `:100`); the cache elsewhere holds placeholder link bytes (`Postgres.hs:729`).
  - Journal: delegate in `QStore` (`MsgStore/Journal.hs:304-361`).
- **Store log** (`Server/StoreLog.hs`): a key-changing swap logs a new record `CreateLinkKeys RecipientId LinkId QueueLinkData (NonEmpty RcvPublicAuthKey)`, tag `"LKEYS"` (the parser is an ordered `A.choice`; the tag must not start with `LINK` or `KEYS`), and then the existing `UpdateKeys`; replay (`StoreLog/ReadWrite.hs:40-79`) runs `addQueueLinkData` then `updateKeys`. A swap without keys logs the existing `CreateLink` with the full `QueueLinkData`. A restart never restores data without its keys; an older server skips `LKEYS` but still restores the key set.
- **Client** (`Client.hs`): `swapSMPQueueLink :: SMPClient -> NetworkRequestMode -> RcvPrivateAuthKey -> RecipientId -> LinkDataHash -> Maybe (NonEmpty RcvPublicAuthKey) -> EncUserDataBytes -> ExceptT SMPClientError IO ()`, gated `thVersion (thParams c) >= linkSwapSMPVersion`, else `PCETransportError TEVersion` (as `Client.hs:1080-1084`).

### 4.2 simplexmq: agent

Synchronous calls; nothing new is stored.

| Function | Does |
|---|---|
| `getConnShortLinkForWrite :: AgentClient -> NetworkRequestMode -> UserId -> ConnShortLink 'CMContact -> AE (FixedLinkData 'CMContact, ConnLinkData 'CMContact, LinkDataHash)` | the contact branch of `getConnShortLink'` (`Agent.hs:1337-1356`), proxied as `LGET` is, also returning the hash of the received user data; when not proxied it runs on the link session (below) |
| `setConnShortLinkSwap :: AgentClient -> NetworkRequestMode -> ConnId -> LinkDataHash -> Maybe (NonEmpty RcvPublicAuthKey) -> UserConnLinkData 'CMContact -> AE ()` | the creator's own link queue: validate owners, sign and encrypt as `prepareContactLinkData` does (`Agent.hs:1247-1268`, user-data part shared), `LSWP` with `rcvPrivateKey` |
| `setShortLinkSwapWithCreds :: AgentClient -> NetworkRequestMode -> UserId -> LinkOwnerCreds -> LinkDataHash -> Maybe (NonEmpty RcvPublicAuthKey) -> UserContactData -> AE ()` | any other owner: require its key in `owners` and `validateLinkOwners rootKey owners` (`Agent/Protocol.hs:2003-2017`), `encodeSignUserData` with its member key, `encryptUserData` with the key from `contactShortLinkKdf linkKey`, `LSWP` signed with its member key |
| `deleteLinkQueueWithCreds :: AgentClient -> NetworkRequestMode -> UserId -> LinkOwnerCreds -> AE ()` | `DEL` with the owner's key, to destroy the channel |
| `getConnLinkQueue :: AgentClient -> ConnId -> AE (SMP.RecipientId, SMP.RcvPublicAuthKey)` | local read of the creator's link queue ID and public key |
| `linkServerSupportsSwap :: AgentClient -> NetworkRequestMode -> UserId -> SMPServer -> SMP.LinkId -> AE Bool` | connect on the link session, compare the negotiated version with v24 |
| `linkServerPrivateRouted :: AgentClient -> UserId -> SMPServer -> AM Bool` | whether the user's settings route commands to this server privately and the connection is not IP-protected (`sendOrProxySMPCommand`'s `shouldUseProxy`, `ipAddressProtected`, `Agent/Client.hs:1190-1211`) |

- `LinkOwnerCreds {server :: SMPServer, linkKey :: LinkKey, rcvId :: SMP.RecipientId, rootKey :: C.PublicKeyEd25519, ownerKey :: C.PrivateKeyEd25519}`, from the channel link, the read's `linkRcvId`, the group's root public key and the member key.
- **Link session.** A non-proxied read, the swaps, `DEL` and the version check use one transport session per link, `(userId, server, Just linkId)` through `withClient`/`withLogClient` (`Agent/Client.hs:1153-1159`); the default session is shared per user and server (`mkTSession`, `:1442-1444`) and would tie them to the user's other queues there. The link ID comes from the link key, so it is known before the read.
- **Lost response.** When a swap fails with a network error or timeout, the agent reads once; if the stored hash equals the hash of the bytes it sent, the call succeeds; otherwise the original error is returned.
- **Direct connections.** The creds calls and `linkServerSupportsSwap` fail with `CMD PROHIBITED` when the user prohibits direct connections to unknown servers (`smpProxyFallback = SPFProhibit`) and the connection is not IP-protected.
- `Crypto/ShortLink.hs`: extract and export `mkOwnerAuth :: OwnerId -> C.PublicKeyEd25519 -> C.PrivateKeyEd25519 -> OwnerAuth`; `newOwnerAuth` (`:81-85`) uses it.
- Errors chat sees: `SMP _ CONFLICT`; `SMP _ AUTH` (key not in the set); `BROKER _ (TRANSPORT TEVersion)` (server below v24); `CMD LARGE` (user data over 13,784 bytes); `CMD PROHIBITED` (direct connection prohibited).
- `DEL` with an unauthorised key deletes the queue record locally (`deleteQueueRec`, `Agent.hs:3080-3088`); after an atomic leave or removal no write restores the key, so the creator's normal link deletion is safe.

### 4.3 Link data, owners, modes

`GroupShortLinkData` (`Protocol.hs:1621-1631`) gains optional fields; `UserContactData` is unchanged and a write carries its `direct` and `ratchetKeys` from the read.

| Field | Type | Written | Meaning |
|---|---|---|---|
| `linkVersion` | `Maybe Int64` | every channel link write: in a swap read + 1; in a single-owner write `link_data_version` + 1, persisted when the write is made (async writes: when queued) | write counter; absent means not gated |
| `revokedOwners` | `Maybe [MemberId]` | removal and leave | departed owners; never shrinks |
| `creatorQueueKey` | `Maybe CreatorQueueKey {memberId :: MemberId, key :: SMP.RcvPublicAuthKey}` | the creator, in every swap from its first promotion | its link-queue recipient key |
| `linkRcvId` | `Maybe SMP.RecipientId` | the creator, in every swap from its first promotion | queue address for other owners; authorises nothing |
| `roster` | `Maybe LinkRoster {version :: VersionRoster, digest :: FD.FileDigest, author :: MemberId}` | every roster claim and the first promotion | the committed roster version, SHA-512 of its blob, its author |

Keys, IDs and `OwnerAuth` encode in JSON as base64url of their `smpEncode` (`strToJSON`/`strParseJSON`). A write encodes the record and merges it over the read JSON object (left-biased union), keeping keys this version does not know; `GroupShortLinkData` today drops unknown fields on parse.

**Current owner** (`currentOwner m = memberRole' m == GROwner && not (memberRemoved m)`) is used for every owner check below. Owner rows created from the chain or a commit have status `GSMemUnknown` (`createLinkOwnerMember`, `Store/Groups.hs:3575-3603`), which `memberCurrent` excludes.

**Multi-owner mode** (`isMultiOwnerChannel`): this device holds an owner row other than its own membership (any status), or has an owner invitation outstanding (`owner_inv_msg_id` set on any member). From the first invitation every link write of the device goes through §4.4; single-owner channels keep `setGroupLinkData`/`setGroupLinkDataAsync` (`Internal.hs:1588-1604`) and add only `linkVersion`.

### 4.4 Writes, reads, follow-ups

**Locks.** `ChatController.linkWriters :: TMap GroupId Lock` serialises this device's link writes per channel. Network calls never run under the group lock (`withGroupLock`), which every agent event of the channel takes on the single event thread (`Subscriber.hs:146-147`, `Store/Connections.hs:67-68`); DB steps take it briefly.

**`writeChannelLink :: User -> GroupId -> ChannelLinkChange -> (ChannelLink -> CM a) -> CM a`**, under the link writer lock:

1. Read with `getConnShortLinkForWrite` on `publicGroup.groupLink`; decode `UserContactData` and `GroupShortLinkData` with its raw JSON.
2. Under the group lock: `applyChannelLinkData` (below), then `applyLinkChange :: ChannelLinkChange -> ChannelLink -> CM (Either ChannelLinkError (Maybe ChannelLink))`; `Right Nothing` = already applied, no write. If the read shows this device departed: stop with `CLENotOwner`.
3. When the owner set changed, `linkQueueKeys`: `creatorQueueKey.key` unless the creator is revoked, plus the `ownerKey` of every other chain entry not in `revokedOwners`; the last-owner check keeps it non-empty.
4. `linkVersion` = read (0 if absent) + 1; `channelLinkWriteHook` (tests); swap: with `GRKPrivate` the creator uses `setConnShortLinkSwap`; any other owner `setShortLinkSwapWithCreds`.
5. `CONFLICT` → from step 1, at most `channelLinkWriteAttempts` (5), then `CLELinkBusy`. `AUTH` → one more read and apply (it shows the revocation), `CLENotOwner`. `TEVersion` → `CLELinkServerVersion`. `CMD LARGE` → `CLELinkDataTooLarge`. `CMD PROHIBITED` → `CLEDirectConnection`.
6. On success, and on "already applied" (no write), under the group lock: store `link_data_version` and run the callback with the written (or read) link: local state, events to send, the result.
7. Release the writer lock; fork the follow-ups of step 2 that are not already pending for the channel (`ChatController.linkFollowUps :: TMap GroupId (Set FollowUp)`), each a further write queued on the writer lock. A write never re-arms the follow-up kind it is running.

Callers never hold the group lock across this call; message-triggered writes are forked. Commands that hold `withGroupLock` across their body today and must take it only around their DB steps: `APIMembersRole` (`Commands.hs:2984`), `APIRemoveMembers` (`:3123`), `APILeaveGroup` (`:3236`), `APIDeleteChat` (`:1390`), `APIAddGroupRelays` (`:2781`).

`FollowUp` constructors are added by the slices that handle them: `FUBecomeOwner` (S8), `FUResign` (S10), `FURemovalWins`, `FUOwnDeparture` (S11).

**Changes:**

| `ChannelLinkChange` | Applies | No write / refuses |
|---|---|---|
| `CLCProfile base new` | the profile, whole | read = `new` → no write (landed); read ≠ `base` → `CLEChangedByOwner` |
| `CLCAddOwner member ownerAuth` | entry after the signer's; key into the set; first promotion also `creatorQueueKey`, `linkRcvId`, `roster` (§4.6) | member revoked → refuse; already in the chain → no write |
| `CLCRevokeOwner memberId` | memberId into `revokedOwners`; its key out | already revoked → no write; no other current owner would remain → `CLELastOwner` |
| `CLCRelays added removed` | read relays + added − removed − relays held as left | no change → no write |
| `CLCCount n` | `publicGroupData.publicMemberCount = n` | not the first current chain entry, or read count = `n` → no write |
| `CLCRoster claim` | `roster` (§4.10) | §4.10 |

Chain rules: signing order is kept (`validateLinkOwners`); the creator signs new entries with the root key (`Commands.hs:2757`), others with their member key (`mkOwnerAuth`); a revoked entry is dropped once no current entry depends on it, transitively, except the writer's own entry in its leave write (a non-root writer must be in the chain, `Agent/Protocol.hs:1996-2001`). An entry is about 124 bytes of the 13,784-byte user data.

**`applyChannelLinkData`**, under the group lock, on every channel link read:

| Party | When |
|---|---|
| member | channel open (`APIGetUpdatedGroupLinkData`, `Commands.hs:1959-1972`), connect plan (`:4624`, `:4651`) |
| owner | channel open, chat start, step 2 of every write |
| relay | invitation (`getLinkDataCreateRelayLink`, `Subscriber.hs:4585-4606`), every relay check (`checkRelayServedGroups`, `Commands.hs:6001-6020`), owner-change checks (§4.13) |
| joiner | `APIConnectPreparedGroup` (`Commands.hs:2303-2333`) |

1. A read whose `linkVersion` is below `link_data_version` is ignored; else store it.
2. Profile and count as `updateGroupFromLinkData` (`Internal.hs:1633-1654`).
3. `revokedOwners`: each held member becomes departed (`GSMemLeft`) at whatever role it is held; unknown memberIds get departed rows. A relay keeps a departed owner's connection until that owner's `x.grp.leave` or a remover's `x.grp.mem.del`, because its last blob can be queued on it. Own memberId listed while the membership is current: follow-up "complete own departure" (§4.9 step 3).
4. Owner rows held as current but absent from the non-revoked chain become departed (an owner exists only through a chain entry; this also ends an owner committed without a link write).
5. Non-revoked chain entries: a member held neither as owner nor departed becomes owner with `ownerKey` (created with a placeholder profile if unknown); one held as departed: on an owner, follow-up "removal wins" (§4.8); its own entry with its own key and a current membership: become owner (§4.6); with another key: alert item, stay a subscriber.
6. `roster` → `link_roster_version`, `link_roster_digest`; `link_roster_seen_at` = now when the version changes. On an owner: if the read author is departed, or is itself with `stored_roster_version` below the read version, follow-up re-sign (§4.10).
7. Owners: join each listed relay without a relay member (`connectToRelayAsync`, `Internal.hs:1606-1616`); `RSInactive` for active `group_relays` rows missing from the list (as `Subscriber.hs:1466-1477`).

**Reconcile** (owners) = read and apply; when the read `roster.version` is above `applied_complete_roster_version`, `XGrpRosterRequest` to every current relay. Every channel owner reconciles on channel open (single-owner creators too, which refreshes `link_data_version` after a restore). Main-app chat start runs it for multi-owner channels (§4.15).

### 4.5 Events and wire

`multiOwnerVersion = VersionChat 23`; `currentChatVersion` 22 → 23 (`Protocol.hs:96-97`).

| Event | Payload | Path | Signed | Forwarded |
|---|---|---|---|---|
| `x.grp.owner.inv` | `{memberId}` | owner → relays → invitee's support scope | yes | `DJSMemberSupport` of the invitee |
| `x.grp.owner.acpt` | `{invId :: SharedMsgId}` | invitee → relays → its support scope | yes | `DJSMemberSupport` of the sender |
| `x.grp.owner.add` | `{memberId, ownerAuth :: OwnerAuth, profile :: Profile, linkVersion :: Int64}` | owner → relays → all | yes | `DJSGroup` |
| `x.grp.members.req` | `{cursor :: Maybe MemberId}` | owner → one relay | no | no |
| `x.grp.members.page` | `{members :: [{memberInfo :: MemberInfo, memberRestrictions :: Maybe MemberRestrictions}], cursor :: Maybe MemberId}` | relay → owner | no | no |
| `x.grp.info` (changed) | adds optional `linkVersion` | as today | yes | yes |

Each event: constructor, `CMEventTag`, tag string and parser, `toCMEventTag`, JSON params (`Protocol.hs:460-1557`), `requiresSignature` (`:1357-1371`), `isForwardedGroupMsg` (`:528-549`), relay dispatch (`Subscriber.hs:1080-1132`), forwarded dispatch (`:4041-4064`), and the Bot API docs (`bots/src/API/Docs/{Commands,Responses,Events,Types}.hs`, with the generated `bots/api/*.md`, TypeScript and Python types, checked by `tests/APIDocs.hs`). Older clients ignore them.

### 4.6 Promotion with consent

**Invite.** `APIInviteGroupOwner {groupId, groupMemberId}` (CLI `/_owner invite #<groupId> <groupMemberId>`, `/owner invite #<group> <member>`) → `CROwnerInvitationSent {user, groupInfo, member}`. Refused unless:

- the user is a current owner of a channel; M is a current member with a key, not an owner, not departed;
- M's code is verified: `memberVerifiedCode` equals `getChannelMemberCode` now (`Commands.hs:3811-3815`), else `CLENotVerified` (a relay that substituted M's key would sign the acceptance with it; only the out-of-band check detects it);
- every current relay supports `multiOwnerVersion`, else `CLERelayVersion`;
- `linkServerSupportsSwap`, else `CLELinkServerVersion`.

It sends `x.grp.owner.inv {memberId}` and stores the message's shared msg id in M's `owner_inv_msg_id` (a new invitation replaces it), which puts the device in multi-owner mode. M's version is not checked (known only from M's join, `Store/Groups.hs:2533-2551`); an older M ignores the invitation.

**Receivers.** Relay: sender a current owner, M current → forward to M's support scope. M: author a current owner and the memberId its own → item `RGEOwnerInvitation {invStatus = OISPending}` in its own support scope (`mkGetMessageChatScope`, `Internal.hs:1984-2003`, which also sets `supportChat`). Others: no item.

**Accept.** `APIAcceptGroupOwnerInvitation {groupId, chatItemId, allowDirect :: AllowDirect}` (`AllowDirect = ADAsk | ADYes`; CLI `/_owner accept #<groupId> <chatItemId>[ direct=on]`) on M: the item must be a pending invitation, the membership current, not owner. When the user prohibits direct connections and the connection is not IP-protected: `CLEDirectConnection {host, prohibited = True}`, whatever `allowDirect` says. With `ADAsk`, if `linkServerPrivateRouted` for the link's server: `CLEDirectConnection {host, prohibited = False}`, and the app asks the user and repeats with `ADYes`. It sends `x.grp.owner.acpt {invId}` (the item's shared msg id), sets the item to `OISAccepted`, returns `CRChatItemUpdated`. A relay forwards it to M's support scope; the promoter acts when `invId` equals M's `owner_inv_msg_id`.

**Promote.** On the acceptance the promoter sets M's `owner_inv_accepted = 1` and, in the main app, forks `promoteOwner`:

1. Re-check M (current, not revoked) and the relays' versions; a failure that persists clears the invitation and is shown to the user. M already a current chain owner means the swap landed before an interruption: go to step 4.
2. First promotion (the creator, link without `creatorQueueKey`): if `roster_version` is set, set `applied_complete_roster_version` to it, rebuild the blob from current roster members without M (`buildGroupRoster`), store it as its own (§4.10) and commit it as `roster {version, digest, self}`; with no roster version, commit none (the first claim takes version 0).
3. `writeChannelLink` with `CLCAddOwner M (mkOwnerAuth …)`.
4. Callback (also when the write was already applied): clear `owner_inv_msg_id` and `owner_inv_accepted`; M's role owner (its key is the chain key); send `x.grp.owner.add {memberId, ownerAuth, profile, linkVersion}` with M's redacted profile (`redactedMemberProfile`, `Internal.hs:1361`) and the link's `linkVersion`; first promotion: send the stored blob to current relays at its version (`sendStoredRoster`, §4.10); item `SGEMemberRole … GROwner`; `CEvtMemberRole`.

An unfinished promotion (`owner_inv_accepted = 1`) runs again at main-app chat start.

**Commit.** Members and owners (forwarded) apply `x.grp.owner.add` when: the sender is a current owner; `ownerAuth.ownerId == memberId`; `ownerAuth` verifies against the root key or an owner key held, current or departed (a delegated entry stays valid after its signer leaves); `memberId` is not held as departed. Then role owner, `member_pub_key = ownerKey` (warning if it replaces a different key), the member created from `profile` if unknown, the role item as `xGrpMemRole` creates it, and `link_data_version = max(stored, linkVersion)`, so a read older than the promotion is ignored rather than making M departed (§4.4 step 4). A relay sends its capabilities (`XGrpRelayCap`) to the new owner. An owner holding `memberId` as departed runs "removal wins" (§4.8). It is not `x.grp.mem.role … owner`, so older clients ignore it and no relay-asserted key is promoted (`Subscriber.hs:3465-3468`).

**M becomes owner** when it applies the commit about itself or reads its own entry (§4.4), with its own key and a current membership: role owner, item `RGEUserRole GROwner`, `members_catchup = 1`, member catch-up (§4.7). It stores no credential: each write builds `LinkOwnerCreds`.

### 4.7 New owner's member list

- **Request.** `x.grp.members.req {cursor}` to one current relay with a ready connection; `cursor = Nothing` starts from the beginning. A watchdog (`ChatController.memberCatchups :: TMap GroupId (Int, Maybe MemberId)`, a request counter and the cursor) re-requests from the same cursor at the next ready relay when no page arrived within `memberCatchupTimeout` (1 minute). Main-app chat start restarts from the beginning while `members_catchup = 1`.
- **Relay.** Serves a current owner only (silence otherwise; the watchdog moves on). One page: current members (statuses of `getGroupMembersByCursor`, `Store/Delivery.hs:352-390`) with `member_id > cursor ORDER BY member_id` (memberIds are the same on every relay and indexed, `UNIQUE(group_id, member_id)`; the cursor member's own row may be gone), excluding relays and the requester; `memberInfo` with the redacted profile and no image, plus restrictions, as many as fit one message (`maxEncodedMsgLength`, `Protocol.hs:929`); `cursor` = the last packed member's memberId, absent when none follows.
- **Owner.** A page from a current relay while `members_catchup = 1`, in one transaction: a held `GSMemUnknown` member gets its profile and status as `xGrpMemNew` gives them (`updateRosterMemberAnnounced`, `Subscriber.hs:3243-3251`), never role or key; departed and other held members are skipped; new ones are created by `xGrpMemIntro`'s channel rule (`Subscriber.hs:3326-3334`, extracted): roles from member up at the default role without a key (the roster establishes those), subscribers with the relay-reported key, as owners keep it from `XGrpMemNew` today. No per-member link write. Then the next request, or `members_catchup = 0`. A page from a relay asked earlier is applied, but only the relay asked last gets the next request.
- Without images about 40–60 members fit a message: ~2,000 messages for 100,000 members.

### 4.8 Removing an owner

- **Command.** `APIRemoveMembers` with a current owner as the only target, by a current owner, in a channel. `writeChannelLink` with `CLCRevokeOwner X`; callback: mark X removed (rows kept, §4.14) and send `x.grp.mem.del X False Nothing` (`deleteMemsSend`, `Commands.hs:3191-3229`). Removing oneself is a leave. A crash between the swap and the callback sends no announcement; parties learn from link reads, relays within their check interval.
- **Relays** apply and forward `x.grp.mem.del` of an owner only after the link check (§4.13); members apply it from a current owner (`checkRole`, `Subscriber.hs:3783-3786`); a relay forwards it to X first, then deletes X's connection (`:3755-3761`).
- **The removed owner's device.** An owner device applies an `x.grp.mem.del` of itself (own-membership branch, `Subscriber.hs:3734-3746`) only after a read shows itself revoked, since honest removers write first; a read showing it revoked also suffices on its own (§4.4 step 3). It then completes its departure: membership removed, keys erased (§4.9 step 3), catch-up stopped.
- **Removal wins.** An owner that holds member M as departed (left or removed) and sees M as a current chain owner (a commit or a read) runs `CLCRevokeOwner M`, and on a write sends `x.grp.mem.del M`. Owners reject an admin's removal of a member they hold as owner (`checkRole`), so an admin cannot remove an owner through this.
- **Self-revocation.** A device that applies its own departure (removal or leave) while it holds an accepted invitation (`OISAccepted` item) keeps `member_priv_key` and reads the link: if its own entry is current, it revokes it (`CLCRevokeOwner self`); if the entry is absent, it re-reads after `rosterRequestTimeout` and at main-app chat starts, until the entry appears (then revokes) or `rosterAbandonTTL` passes since the acceptance. Then it erases the key and sets the item to `OISClosed`. This settles an admin's removal racing the promotion in either order: before or after the promoter's swap.
- Mutual removal: the first swap removes the other's key; the second gets `AUTH`, or `CONFLICT` and a read showing itself revoked.

### 4.9 Leaving

`APILeaveGroup` by an owner in multi-owner mode (single-owner channels keep `Commands.hs:3233-3277`):

1. Read. Refuse with `CLELastOwner` if no other current owner. If `roster.author` is this owner at its stored version and no relay has `SENT` the latest `x.grp.roster` this device sent (`msg_deliveries`), re-send the stored blob only, with no event (`sendStoredRoster`; an event at that version would mark it complete on members that never got its changes), and wait up to `channelOwnerWaitTimeout` (30 s), without the group lock (`SENT` is processed under it), for `SENT` of the blob's last chunk to a relay; else refuse with `CLERelaysUnreachable`. In channels a message to a relay that is not ready is dropped (`memberSendAction`, `Internal.hs:2840-2846`).
2. `writeChannelLink` with `CLCRevokeOwner self` (own entry kept, key dropped); the last-owner check runs on every attempt.
3. Complete the departure, only while the membership is current: `x.grp.leave` to relays; delete member connections waiting for delivery; membership `GSMemLeft`; erase `member_priv_key` (a later load generates an unrelated key, `Store/Shared.hs:750-762`); the creator replaces `root_priv_key` with the public key and deletes its group link (`deleteGroupLinkIfExists`: its `DEL` gets `AUTH` and deletes locally).

A crash after step 2 is completed by the next read (§4.4 step 3). A repeated leave whose read shows itself already revoked completes step 3 and returns `CRLeftMemberUser`.

### 4.10 Roster ordered by the link

**Claim.** In multi-owner mode `APIMembersRole` and `APIRemoveMembers` call `claimRosterVersion` instead of `broadcastRoster` (`Commands.hs:3005-3008`, `:3136-3139`): `writeChannelLink` with `CLCRoster (RCChange delta targets startRoles)`, where `startRoles` are the targets' roles loaded when the command started. `RosterClaim = RCChange RosterDelta [GroupMember] [GroupMemberRole] | RCResign`. With `r` = read `roster.version` (none = −1) and `f` = `applied_complete_roster_version` (none = −1):

1. If `r > f`: if the read author is this owner (its claim `r` was lost before it was stored or sent), or the author is departed and the abandonment rule holds (below), claim from its own state; otherwise send `XGrpRosterRequest` with its `stored_roster_version` to every current relay and refuse with `CLERosterSyncing`.
2. `RCChange`: any target role ≠ its start role → `CLEChangedByOwner`.
3. Build the blob at `r + 1` from current roster members with the delta (`buildGroupRoster (applyRosterDelta …)`, `Internal.hs:2629-2654`); `roster = {r + 1, SHA-512 of the blob, self}`.
4. Callback: `setOwnGroupRoster` sets `roster_version`, `applied_complete_roster_version`, `stored_roster_version` to `r + 1`, `roster_blob`, sender = own membership (header columns NULL); `sendStoredRoster`; return `r + 1` for the events.

Single-owner channels keep `broadcastRoster`. Owners outside the roster: `processRosterEntries` (`Subscriber.hs:3610-3630`) skips current owners, the user's own membership included, and departed members; `xGrpMemRole` ignores owner targets and the owner role; `APIMembersRole` refuses owner targets and the owner role in channels.

**Stored blob.** `getOwnStoredRoster` returns `(stored_roster_version, roster_blob)`; `sendStoredRoster` sends `XGrpRoster {version, sha512 of the bytes}` and the chunks of those exact bytes (`sendRoster`'s steps without re-encoding, `Internal.hs:2665-2681`). In multi-owner mode a relay gets a roster only this way and only when `stored_roster_version` equals the read `roster.version` (an owner's complete frontier can advance from another owner's events without the blob, so it is not the test); otherwise the owner requests the roster with its `stored_roster_version` (relays serve when theirs is greater) and sends after completion. This covers relay set-up (`Subscriber.hs:937-942`: in multi-owner mode it creates no version; a channel with none first claims version 0 with `RCResign`), the acknowledgement (`xGrpRosterAck`, `:3654-3672`: compared with the version this owner sent that relay; a lower one re-sends once per newer stored version), and every re-send.

**Departed authors.** A blob whose author is departed is accepted only if its version and digest equal `link_roster_version`/`link_roster_digest`:

- `xGrpMsgForward` (`Subscriber.hs:4019-4020`) passes an `XGrpRoster` from a departed owner to `xGrpRoster` on an owner device; other content stays ignored.
- Relay direct path: from a departed owner's connection only `XGrpLeave`, `XGrpRoster` and the `BFileChunk`s of a roster transfer started from that connection (`bFileChunkGroup`, `:2626-2633`) are processed.
- Owners and relays start the transfer (`xGrpRoster`, `:3503-3540`) and check the commitment at completion (`rosterCompletion`, `:3549-3586`); a mismatch triggers one link read (forked; the blob stays pending until it returns) and is applied if the fresh commitment matches.
- Relays answer a current owner's `XGrpRosterRequest` whenever stored > requested (`xGrpRosterRequest`, `:3679-3685`), without the served-version bound kept for members.

**Re-sign** (`CLCRoster RCResign`), a follow-up when the read author is departed or is this owner with a lower stored version (`applyChannelLinkData` step 6), after applying another owner's departure (`x.grp.leave`, `x.grp.mem.del` of an owner, including this device's own removal of the roster author), and after completing a departed author's blob: if `f >= r` claim `r + 1` from the current roster; if step 1 allows, claim from its own state; else request and stop. A re-sign writes only when its read shows the author departed, or itself with a lower stored version, or (at chat start, below) itself with its latest roster undelivered; otherwise, e.g. after another owner re-signed first, it makes no write. It runs only on these triggers and the abandonment re-check, at most one pending per channel (`linkFollowUps`). After a claim it sends the blob and one `x.grp.mem.role` at `r + 1`: the current role of the first current non-owner, non-relay member with a key, by memberId. Any event at `r + 1` makes members that dropped the departed author's events see a gap and fetch the blob (`applyAtRosterVersion` acts before the target lookup, `Subscriber.hs:3410-3424`). The swap lets one owner claim; the others' reads then show a current author.

**Abandonment.** A version `r` by a departed author is abandoned (claim `r + 1` from own state; nobody received `r`) when `link_roster_seen_at` is older than `rosterAbandonTTL` (1 day) and a request sent to every current relay at least `rosterRequestTimeout` (5 minutes) earlier brought no blob at `r`: the re-sign sends that request and forks a re-check after the timeout. An owner that also missed earlier versions of that author (f < r − 1) drops them too, though some devices received them: only a departed author's committed version is accepted, and the alternative is a channel whose roles can never change again.

**Chat start** (main app): an owner that is the link's roster author at its stored version, when no relay has `SENT` its latest `x.grp.roster` (its blob and events were dropped), makes an `RCResign` claim at `r + 1`: the blob and the event at `r + 1` make members that missed `r` see a gap and fetch. It never sends an event at a version it is re-sending.

### 4.11 Subscriber count

In multi-owner mode the owner branch of `updatePublicGroupData` (`Internal.hs:1618-1631`) recounts (`updatePublicMemberCount`) and, when `members_catchup = 0` and in the main app, schedules a publish: `ChatController.countPublishPending :: TMap GroupId ()`; a trigger finding no entry inserts one and forks a thread that sleeps `channelCountPublishInterval` (60 s), deletes the entry and runs `writeChannelLink` with `CLCCount tally`. Only the first current chain entry writes; others make at most one proxied read a minute while the count changes. Main-app chat start schedules one publish per multi-owner channel. The relay-announcement threshold in relay activation becomes `publicMemberCount > number of current owners` (`Subscriber.hs:1445-1449`).

### 4.12 Profile edits and the link counter

- In multi-owner mode `runUpdateGroupProfile` (`Commands.hs:4164-4192`, behind `APIUpdateGroupProfile`, `APISetPublicGroupAccess` and the other profile commands) calls `writeChannelLink` with `CLCProfile base new`, where `base` is the local profile when the command starts; `CLCProfile` compares profiles as `J.toJSON` values. The callback writes the local profile, then `XGrpInfo` goes out with the written `linkVersion`. The command API is unchanged.
- The app's profile editors refuse to save, with the "changed by another owner" alert, when the channel's profile in the app model changed while the editor was open (an `XGrpInfo` from another owner updates the model); comparing the app's own model on both sides avoids false conflicts from fields the apps do not model. The core check covers a change that reached the link but not yet this device.
- `xGrpInfo` (`Subscriber.hs:3840-3865`): an event with `linkVersion` below `link_data_version` is not applied; a higher one is stored. Relays forward every `XGrpInfo` and gate only their own apply.

### 4.13 Relays in a multi-owner channel

- **Irreversible owner actions checked against the link.** In a channel where the relay holds at least two owner rows (any status), it applies and forwards `x.grp.mem.del` of an owner and `x.grp.del` only after a fresh proxied read shows the sender a current chain owner and, for a removal, the target revoked; for `x.grp.del` a read failing with `AUTH` also passes (the queue is gone, and only a current recipient key can delete it). The check never blocks the event thread: the relay saves the message, defers its ACK (`withAckMessage`, `Subscriber.hs:1797-1832`, gains a "defer" result; `ackMessageAsync` acks later), and forks the read, retrying temporary errors with backoff. The deferred ACK holds that connection's later messages, keeping the sender's order, and a restart redelivers the message. After the read, under the group lock, it applies or rejects the event, creates the delivery task, and acks. Members get these events only through relays and check the sender as today. Owner additions are not checked: an addition made without a link write is undone at the relay's next read (§4.4 step 4), and its owner's deletions and removals fail the check.
- **Activation.** The `LINK` handler body (`Subscriber.hs:1437-1477`) becomes `activateGroupRelays :: User -> GroupInfoKeys -> [ShortLinkContact] -> CM ()`, called by the handler and in the callback of a `CLCRelays` write; it reads `group_relays` and the group link row only when present. `CEvtGroupLinkDataUpdated` and `CRGroupRelaysAdded` carry `Maybe GroupLink` (`Controller.hs:862`, `:1035`).
- **Publication** after a relay's acknowledgement (`Subscriber.hs:3661-3665`) and a pre-roster relay's connection (`:946-947`): a forked `CLCRelays [link] []`. Main-app chat start publishes acknowledged relays missing from the link.
- **Removal** of a relay (`Commands.hs:3149-3153`) and a relay's own leave seen by any owner: `CLCRelays [] [link]`.
- **`APIAddGroupRelays`** without a group link row takes the link from `publicGroup.groupLink` (`Commands.hs:2790-2793`). A relay below `multiOwnerVersion` added in multi-owner mode stays invited (its `updateRelayGroupKeys` fails); the owner removes it.
- **Owners join relays added by others** from `XGrpRelayNew` (`Subscriber.hs:3881-3886`) and in reconcile. Relay side: `memberJoinRequestViaRelay` (`:1690-1720`) accepts a key-proven join onto a current owner row, as for roster rows, and refuses joins onto departed rows. Owner side: `updatePreparedUserAndHostMembers'` (`Store/Groups.hs:821-844`) and the host `CON` branch (`Subscriber.hs:892-922`) leave the membership and group profile and the joined items alone when the membership is an owner.
- **Invitation by any owner.** `updateRelayGroupKeys` (`Store/Groups.hs:2353-2373`) creates an owner row (`createLinkOwnerMember`) for every chain owner without one and applies `revokedOwners`; the inviting-owner check (`Subscriber.hs:4598`) excludes `revokedOwners`.
- **Capabilities.** `XGrpRelayCap` (`Subscriber.hs:1103-1105`) from a relay without this owner's `group_relays` row is stored in `group_members.relay_web_domain` (`relayWebDomain` in `GroupMember` JSON); `updateRelayCapabilities` (`Store/Groups.hs:1901-1911`) updates only `group_relays` today. A relay sends `XGrpRelayCap` to an owner when it applies its commit or its join, since `sendRelayCapIfNeeded` (`Internal.hs:2686-2696`) sends only when the group-level domain changes.

### 4.14 Other guards

- **Durable rows.** In channels `getGroupMembersForExpiration` and `getRemovedMembersToCleanup` (`Store/Groups.hs:1357-1382`) skip owner and roster-role rows; an intro (`xGrpMemIntro`, `Subscriber.hs:3320-3322`) never changes a departed member's status.
- **`xGrpDel`** (`Subscriber.hs:3830`) requires a current owner (and, on relays, the link check).
- **Group-link commands** `APICreateGroupLink`, `APIAddGroupShortLink`, `APIDeleteGroupLink` refused for channels (`Commands.hs:3349-3387`).
- **`APIShareChatMsgContent`** signs with any current owner's member key (`Commands.hs:1238-1240`).
- **Connect plan and join** exclude `revokedOwners` from `glOwners`, `verifyLinkOwner` and owner binding (`Commands.hs:4615-4653`, `:4806-4815`); `APIConnectPreparedGroup` creates owners only for non-revoked entries and departed rows for revoked ones (`:2324-2332`).
- **History for joiners.** A signed as-channel item whose author is no longer a current owner is served unsigned (`FwdChannel`, `itemForwardMsgs`, `Internal.hs:1540-1548`).
- **Destroying.** In multi-owner mode `APIDeleteChat` by an owner (`Commands.hs:1384-1401`) first reads and applies the link; a departed device completes its departure and sends no `x.grp.del`; if the read fails, it proceeds as today. A promoted owner then deletes the link queue with `deleteLinkQueueWithCreds`; the creator keeps `deleteGroupLinkIfExists` (current: `DEL` deletes the queue; departed: `AUTH`, local delete).

### 4.15 Main app and chat start

`ChatController.mainApp :: TVar Bool`, set by `startChatController` (`Commands.hs:232-269`). Forks of link writes, reconcile, re-sends and catch-up run only in the main app (the iOS notification extension starts with `mainApp = False`). Main-app chat start (`startChannelOwnerWork`, next to `startRelayChecks`), and `APIActivateChat` with `restoreChat` (`Commands.hs:599-610`; iOS returns to the foreground this way, without a chat start), re-derive everything a skipped or interrupted fork would have done:

| For | Does |
|---|---|
| each multi-owner channel where the user is a current owner | reconcile (read, apply and follow-ups: removal wins, re-sign, own departure); roster re-send (§4.10); publish acknowledged relays missing from the link; schedule a count publish; restart a pending member catch-up |
| each member with `owner_inv_accepted = 1` | `promoteOwner` |
| each channel where the user holds an `OISAccepted` invitation and is departed | self-revocation (§4.8) |

Each slice adds its rows: promotions and catch-up with S8–S9, the roster re-send with S10, self-revocation with S11.

### 4.16 Types, database, configuration

| Kind | Name | Notes |
|---|---|---|
| command | `APIInviteGroupOwner {groupId, groupMemberId}` | → `CROwnerInvitationSent`; record syntax and Bot API docs, as every command |
| command | `APIAcceptGroupOwnerInvitation {groupId, chatItemId, allowDirect}` | → `CRChatItemUpdated` |
| error | `CEChannelLink {channelLinkError :: ChannelLinkError}` | `CLENotVerified`, `CLERelayVersion`, `CLELinkServerVersion`, `CLEDirectConnection {host, prohibited}`, `CLEChangedByOwner`, `CLERosterSyncing`, `CLELastOwner`, `CLENotOwner`, `CLELinkDataTooLarge`, `CLELinkBusy`, `CLERelaysUnreachable` |
| item | `RGEOwnerInvitation {invStatus :: OwnerInvitationStatus}` | `OISPending`, `OISAccepted`, `OISClosed` |
| column | `groups.link_data_version INTEGER` | highest `linkVersion` read or written |
| column | `groups.link_roster_version INTEGER`, `link_roster_digest BLOB` (`BYTEA`), `link_roster_seen_at TEXT` (`TIMESTAMPTZ`) | last read roster commitment; when its version first appeared |
| column | `groups.members_catchup INTEGER NOT NULL DEFAULT 0` | new owner's catch-up pending |
| column | `group_members.owner_inv_msg_id BLOB` (`BYTEA`), `owner_inv_accepted INTEGER NOT NULL DEFAULT 0` | promoter: the pending invitation to this member; accepted, promotion to run |
| column | `group_members.relay_web_domain TEXT` | relay capability for an owner without the relay's row |
| config | `channelLinkWriteAttempts = 5`, `channelCountPublishInterval = 60 s`, `rosterAbandonTTL = 1 day`, `rosterRequestTimeout = 5 min`, `memberCatchupTimeout = 1 min`, `channelOwnerWaitTimeout = 30 s` | `ChatConfig` |
| hook | `channelLinkWriteHook :: Maybe (GroupInfo -> IO ())` | `ChatHooks`, between read and swap, outside the group lock; tests |
| state | `linkWriters :: TMap GroupId Lock`, `linkFollowUps :: TMap GroupId (Set FollowUp)`, `countPublishPending :: TMap GroupId ()`, `memberCatchups :: TMap GroupId (Int, Maybe MemberId)`, `mainApp :: TVar Bool` | `ChatController` |

One migration `M20261015_channel_multi_owner` with `down_m20261015_channel_multi_owner`, SQLite and Postgres, registered in both `Migrations.hs` and `simplex-chat.cabal`; schema dumps regenerate from tests.

### 4.17 Races and where each is ordered

| Race | Ordered by | Where | Outcome |
|---|---|---|---|
| Two link writes of any kind | the swap | `swapQueueLinkData` | the second re-reads and re-applies, or is refused |
| A promotion by O1 while O2 removes O1 | the swap (keys and data together) | `writeChannelLink` | removal first: O1 gets `AUTH` or `CONFLICT` and reads itself revoked; else the removal keeps M's key |
| Two owners remove each other | the swap | §4.8 | one remains |
| A removal and a leave; the last two owners leaving | the swap; last-owner check per attempt | §4.4 | one current owner remains |
| Two role changes | `roster.version` | `claimRosterVersion` | the second catches up and claims `v + 2`, or is refused for the same target |
| A role change just before leaving | the commitment, the leaver's wait for `SENT` | §4.9, §4.10 | the committed blob is accepted from the departed author; the re-sign reaches members |
| Two profile edits | the editor's check and the swap; `linkVersion` | §4.12 | the later is refused if the profile changed since its editor opened; members keep the newer |
| A relay added while another is removed | the swap | `CLCRelays` | both kept |
| A member removed or leaving while promoted | removal wins; self-revocation | §4.8 | the member ends departed |
| A removed owner acting before relays learn | the relay's deferred link check | §4.13 | owner removals and deletion refused; an owner it adds without a link write is undone at the relay's next read; posts and role events possible until the relay learns |
| A read older than a promotion applied after its commit | the commit's `linkVersion` | §4.6 | the read is ignored; M stays an owner |
| A party that was offline | link data on every read | §4.4 | converges at its next read |
| A single-owner write queued before the first promotion | `LSET` refused on a multi-key queue | §4.1 | the late write gets `AUTH`; nothing is overwritten |

## 5. Compatibility and security

- **Wire.** SMP v24 (`LSWP`, `CONFLICT`, `LSET` refused on multi-key queues); a v23 server gets no `LSWP`. Chat v23 events (§4.5); `XGrpInfo` gains an optional field. Two new API commands, one new response and new types, documented in the Bot API docs.
- **Database.** One migration, columns only, both backends; existing rows get NULL or 0. No agent or server schema change.
- **Server downgrade.** An older server skips `LKEYS` lines and restores keys from the `KEYS` lines; it loses those swaps' data since its last start and accepts `LSET` again (multi-owner writes then stop: there is no `LSWP`).
- **Creator downgrade.** An older creator's `LSET` on the multi-key queue gets `AUTH`; it cannot update the link.
- **Older parties.** Promotion needs every relay at chat v23 and the link server at SMP v24 (link servers, presets included, upgrade first). Older members ignore `x.grp.owner.add` and reject the new owner's signed messages until they upgrade; older joiners take every chain entry as an owner, including revoked ones, whose keys authorise nothing on the link queue or at relays.
- **Adversary.**
  - Relay: cannot forge owner-signed events, commits or acceptances; can withhold or reorder as today; cannot substitute a promoted member's key (verified code); can fabricate subscribers in a page, never privileged ones.
  - Link server: cannot forge link data; a stale read is ignored by `linkVersion`; ignoring the compare-and-swap brings back lost writes and roster divergence, no forgery.
  - Removed owner: its key leaves the set in the removing write; relays refuse its owner removals and deletion after a link check, and undo an owner it adds without a link write at their next read; owners and relays accept its blobs only when committed; until a relay learns of the removal it can still post and send role events there; with a colluding link server it can serve forged link data to readers.
  - Any current owner can destroy the channel or replace the key set with `RKEY`.
- **Privacy.** A promoted owner connects directly to the link server (recipient commands are not proxied) for its user's actions and, as first chain entry, for the count; acceptance names the host and the IP exposure and needs `ADYes` when the settings would route privately; direct-prohibited settings refuse the writes. Link commands use a session of their own; link reads stay proxied. The link server learns when role changes happen. `linkRcvId` and `creatorQueueKey` authorise nothing. The member-page cursor is a memberId, not the relay's row ID.

## 6. Slices

Each slice compiles and passes its tests on its own. simplexmq: `cabal test simplexmq-test --test-options='-m "<pattern>"'` (Postgres with `-fserver_postgres`, on the host); chat: `cabal test simplex-chat-test --test-options='-m "channels"'`, with `use-local-simplexmq` until simplexmq is pinned.

| # | Slice | Est. | Done when |
|---|---|---|---|
| S1 | simplexmq store: `LinkDataHash`, `CONFLICT`, `swapQueueLinkData` (STM, Postgres, journal), `CreateLinkKeys` + `UpdateKeys` logging and replay | ~170 | store, store-log and Postgres store tests |
| S2 | simplexmq protocol: v24, `LSWP`, handler, `LSET` refusal on multi-key queues, client, protocol doc | ~110 | server tests |
| S3 | simplexmq agent: §4.2 | ~190 | agent tests |
| S4 | chat structural: extract `activateGroupRelays`, the channel intro member creation | ~80 | `-m "channels"` green, no behaviour change |
| S5 | chat wire and data (inert): migration, link fields, events, versions, errors, commands in record syntax (refusing), config, hook, state; Bot API docs and regenerated files | ~300 | `-m "channels"`, protocol, JSON and "Bot API docs" tests |
| S6 | link core: locks, `writeChannelLink` and `applyChannelLinkData` with no follow-up kinds yet, reconcile, `currentOwner`, multi-owner mode, `linkVersion`, `XGrpInfo` gating, durable rows, intro guard, relay reads and tolerant invitation, main-app gating | ~250 | T12, existing tests |
| S7 | callers: profile check (core and the editors' rule in §6.1), relays (activation, publication, removal, owner joins, capabilities), count publisher and threshold | ~200 | existing tests |
| S8 | promotion: invite, accept with the direct-connection check, `promoteOwner` with the stored-blob store and send, commit, M becomes owner (`FUBecomeOwner`), chat-start rows for promotions | ~260 | T1 (without the role change), T2, T5 (profile, relays), T13, T20 |
| S9 | member catch-up with watchdog | ~120 | T10 |
| S10 | roster: claim, owners outside the roster, stored-blob rules, departed authors, re-sign (`FUResign`), abandonment, relays serving owners, chat-start re-sign | ~250 | T1 role change, T5 roles, T21 |
| S11 | removal and leave: removal, removal wins (`FURemovalWins`), own departure (`FUOwnDeparture`), self-revocation, leave with wait, removed device, destroy, relays' deferred checks, departed-owner guard | ~280 | T3 (without the history check), T4, T6–T9, T11, T14–T19, T22 |
| S12 | guards: group-link commands, share signing, connect plan, history | ~70 | existing tests, T3's history check |
| S13 | iOS (§6.1) | ~400 | host build, manual checks |
| S14 | Kotlin, mirroring S13 | ~400 | host build, manual checks |

### 6.1 Apps (iOS and Kotlin, mirrored)

Same file split, names and branch order on both platforms.

| Change | iOS | Kotlin |
|---|---|---|
| API: `apiInviteGroupOwner`, `apiAcceptGroupOwnerInvitation(allowDirect)`, `ownerInvitationSent`; `groupLink` optional in `groupRelaysAdded`, `groupLinkDataUpdated`, `AddGroupRelaysResult.added` | `SimpleXAPI.swift`, `AppAPITypes.swift:1001, 1210` | `SimpleXAPI.kt:6815, 6846` |
| Errors: `channelLink(ChannelLinkError)`, one alert per case; SMP `CONFLICT` | `APITypes.swift:744+` | `SimpleXAPI.kt:7807+` |
| `RcvGroupEvent.ownerInvitation(invStatus)`: text, notification, bold like `newMemberPendingReview`; tapping a pending one asks "Become an owner?" (the member's profile goes to all subscribers; the device connects directly to <host>, revealing its IP address, for channel changes and the count) → accept; on `directConnection`, confirm and repeat with `allowDirect`, or, when `prohibited`, explain that the network settings must allow direct connections; an `OISClosed` item shows no action | `ChatTypes.swift:6054+`, `ChatItemView.swift:153-157` | `ChatModel.kt:5447+`, `ChatItemView.kt:734-748` |
| Role picker: Owner for a current owner and a current non-owner member with a key; unverified → verification (`APIVerifyGroupMember`); else confirm (member is asked; owners can remove owners; any owner can delete the channel) → `apiInviteGroupOwner`; no picker for owner targets | `ChatTypes.swift:3382-3394`, `GroupMemberInfoView.swift:327-331, 731-768` | `ChatModel.kt:2944-2955`, `GroupMemberInfoView.kt:149-158, 947-991` (roles list keyed so it recomputes) |
| Remove: "Remove owner" label and confirmation | `GroupMemberInfoView.swift:697-707`, `GroupChatInfoView.swift:995-1044` | `GroupMemberInfoView.kt:843-854`, `:253-334` |
| Leave in the channel info view when another current owner exists; core refusals shown (the chat list keeps today's rule: it has no members loaded) | `GroupChatInfoView.swift:249-254`, `SimpleXAPI.swift:2047-2060` | `GroupChatInfoView.kt:828-838`, `SimpleXAPI.kt:2493-2498` |
| Link screen: an owner without a `GroupLink` sees the published link; never creates a link for a channel; SimpleX-name setting for any current owner | `GroupChatInfoView.swift:113-127, 162-170, 402-403`, `GroupLinkView.swift:164-166` | `GroupChatInfoView.kt:599-611, 642-653, 670-681`, `GroupLinkView.kt:119-123` |
| Channel open: owners also call `apiGetUpdatedGroupLinkData` | `ChatView.swift:762-780` | `ChatView.kt:225-242` |
| Profile editors keep the channel profile they opened with and, on save, refuse with the "changed by another owner" alert if the model's profile differs | `GroupProfileView.swift`, `GroupWelcomeView.swift:145`, `GroupPreferencesView.swift:227`, `GroupChatInfoView.swift:1143`, `ChannelWebAccessView.swift:157` | `GroupProfileView.kt`, `WelcomeMessageView.kt:48`, `GroupPreferences.kt:49`, `MemberAdmission.kt:40`, `ChannelWebPageView.kt:59` |
| Composer: an owner without `group_relays` rows uses the relay-member state | `ComposeView.swift:739-773` | `ComposeView.kt:2151-2177` |
| Relays: add-relay excludes relay members' links too; web settings take domains from rows and relay members' `relayWebDomain` | `ChannelRelaysView.swift:38-41`, `ChannelWebAccessView.swift:86-120` | `ChannelRelaysView.kt:109-137`, `ChannelWebPageView.kt` |
| Strings | `en.xliff` via `NSLocalizedString` | `moko-resources/base/strings.xml` |

Not compiled in the container; built on the host.

## 7. Test plan

**simplexmq** (`tests/ServerTests.hs` per store, `tests/CoreTests/StoreLogTests.hs`, `tests/CoreTests/MsgStoreTests.hs`, `tests/AgentTests/FunctionalAPITests.hs`):

- `LSWP`: right hash replaces user data and keeps fixed data; wrong hash `CONFLICT`, nothing changed; no link `AUTH`; messaging queue `AUTH`; via proxy `CMD PROHIBITED`; v23 session cannot send it; 52 Ed25519 keys fit.
- With keys: the removed key gets `AUTH`, the added key writes; `LSET` on a two-key queue gets `AUTH`; concurrent swaps: one `OK`, one `CONFLICT`, the second then succeeds (Postgres-only store included).
- Restart: a key-changing swap replays data and keys; a data-only swap replays as `LINK`; a log without `LKEYS` lines still restores the keys.
- Agent: the read's hash is accepted by both swaps; an owner-signed swap is read and verified by `getConnShortLink`; a removed signer gets `AUTH`; a dropped swap response is recovered by the read; `deleteLinkQueueWithCreds`; `linkServerSupportsSwap` false on a v23 server.

**chat**, `describe "multiple owners"` under `describe "channels"` (`tests/ChatTests/Groups.hs:281`), relays as in `createChannel2Relays` (`:9616`):

| # | Test |
|---|---|
| T1 | verify, invite, accept; an earlier subscriber accepts M's signed post; a member that knew M through a relay-asserted key ends with the chain key; M edits the profile and changes a role |
| T2 | refusals: unverified; a relay below v23; a link server at SMP v23; M ignores a chain entry with another key |
| T3 | creator promotes M and leaves; M posts through every relay, edits the profile, adds and removes a relay, promotes a verified subscriber, destroys the channel (relays and subscribers process the deletion); a later joiner sees M as owner, the creator departed, verifies the roster, gets the creator's history unsigned |
| T4 | O1 removes O2: O2's device shows the removal, its stored key no longer matches its entry, its next write fails; members and a new joiner treat O2 as departed |
| T5 | with the hook holding O1 between read and swap: O2's profile edit lands, O1's is refused; O1's edit while O2's `XGrpInfo` has not reached it yet is refused; O1 adds and O2 removes relays, both kept; O1 and O2 change roles of different members, every relay and member ends with both |
| T6 | mutual removal: one owner remains |
| T7 | the last two owners leave: one refused |
| T8 | removal wins: O2 removes subscriber M while O1's promotion of M is held by the hook; M ends departed everywhere |
| T9 | O1 demotes the only admin and leaves while O2 is offline; O2 returns, accepts O1's committed blob, re-signs; members that dropped O1's events fetch on the gap; a joiner gets the re-signed roster; the demotion holds |
| T10 | catch-up: earlier subscribers appear on M with keys (it verifies and promotes one); a restart mid-way restarts and skips held members; a silent relay is replaced after the timeout and no member is skipped |
| T11 | count: after the creator leaves, joins and leaves change the published count within the (test-shortened) interval |
| T12 | `linkVersion`: a lower `XGrpInfo` is not applied but relays forward it; a stale read does not revert the profile |
| T13 | a single-owner async write queued before the first promotion lands after it and gets `AUTH`; the link keeps M |
| T14 | chain dependency: creator → M → N; M leaves (its entry stays as N's signer); N leaves; writes keep validating |
| T15 | a relay reads the creator's departure before processing its queued blob and still accepts it |
| T16 | a leave stopped after its swap (hook) completes at the next chat start; abandonment after the (test-shortened) TTL claims the next version |
| T17 | relays refuse a removed owner's `x.grp.mem.del` of an owner and `x.grp.del` sent after its removal, and undo an owner it adds without a link write at their next check; its link `DEL` gets `AUTH`; while a delayed read holds a relay's check, its other channels keep delivering |
| T18 | an admin removes M while O1's promotion of M is held: M's device revokes itself; M ends departed everywhere |
| T19 | destroy by a device that left after its swap sends no `x.grp.del` |
| T20 | a read held (hook) across a promotion is applied after the commit: M stays an owner everywhere |
| T21 | O2 adds a relay right after O1's role change: the relay gets the current blob, acknowledges and is published |
| T22 | relays not ready when O1 demotes an admin: O1's chat start re-signs at the next version and members fetch it; the same with O1 leaving instead (blob only, then O2's re-sign) |

Existing tests that must stay green: the whole `-m "channels"` group, `ProtocolTests`, `JSONTests`, "Bot API docs", schema dump. Final: `cabal test simplex-chat-test` and `simplexmq-test` (Postgres on the host).

**Manual app checks** (one layout; nothing here depends on layout): invite and accept on two devices with the direct-connection prompt, Owner option and verification routing, Remove owner, last-owner leave refusal, a promoted owner's link screen, composer, relays and web settings, a profile edit refused after another owner's edit.

## 8. Risks

- **Riskiest assumption:** members' and relays' unchanged roster protocol converges when versions are claimed through the link. T5, T9, T15, T21 and T22 in S10–S11; S1–S2 first prove the swap's atomicity and size.
- The departed-author exception narrows a drop rule that protects members: roster blobs only, on owners and relays, matching a committed digest.
- Rollout: link servers (presets included) must run v24, and relays chat v23, before promotion works.
- Postgres server tests and app builds run only on the host.

## 9. Open questions

1. The link server must be reachable for every role change in a multi-owner channel (1–3 s). Decided by the user: accepted.
2. Consent and key-substitution defence: verified code plus an invitation in the channel. Decided by the user.
3. Member list: pulled from one relay, no images. Decided by the user.
4. `LSWP` sends only the user data. Recommended: as planned; otherwise owners are capped at about 6.
5. `LSET` is refused on queues with several recipient keys (a server behaviour change at v24). Recommended: as planned; it is what stops stale or downgraded single-owner writes from erasing other owners.

## 10. Out of scope

Pre-existing single-owner issues: relay forwarding order (`Store/Delivery.hs:178-224`); messages to a backlogged relay dropped (`Internal.hs:2840-2846`); a queued async link write overwriting a newer one in single-owner channels (`Agent.hs:1191-1200`); moderator and admin promotion using relay-reported keys (`Commands.hs:3053`); block and unblock not ordered.

## 11. Follow-up runs

None: the whole feature is planned here.
