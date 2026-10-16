# Channel multi-owner (S3-min) — implementation plan

Branch: `f/channel-multi-owner` · Repos: simplexmq (from pinned `cba13823`), simplex-chat · Estimated diff: ~2,150 production Haskell lines (simplexmq ~480, chat ~1,670) plus tests, and ~380 lines per app platform.

The overview, `2026-10-15-channel-multi-owner-s3min.md`, says what owners can do and why; this file says what to build. Anchors are simplex-chat paths under `src/Simplex/Chat/` and simplexmq paths under `src/Simplex/Messaging/` at `cba13823`.

## 1. Goal and acceptance criteria

A channel has several equal owners. Any owner can do what the single owner does today; owners are added with the member's consent, can remove each other and can leave; concurrent owner actions are ordered by the link server, so none is lost, overwritten unseen or reverted. Owners trust each other as the single owner is trusted today; the design protects against malicious relays and against a link server that withholds or serves stale data.

| # | Criterion | Tests (§7) |
|---|---|---|
| AC1 | An owner invites a member whose code it verified; the member accepts; one link write adds the member's chain entry and recipient key; every relay and subscriber treats it as owner; it posts, edits and changes roles | T1, T20, T21 |
| AC2 | Promotion is refused for an unverified member, while a relay is below `multiOwnerVersion`, and on a link server below SMP v24 | T2 |
| AC3 | The creator promotes M and leaves; M does every owner action; a later joiner sees M as owner and the creator as departed, verifies the roster and gets the creator's history unsigned | T3 |
| AC4 | An owner removes another; the removed owner's link writes and `DEL` get `AUTH`; its device shows the removal and erases its keys; relays, members and joiners treat it as departed | T4, T15 |
| AC5 | Concurrent link writes both land or one is refused; a profile edit over a profile changed since the editor opened is refused; concurrent role changes reach every relay and member and no demotion is reverted; mutual removal and the last two owners leaving each leave one current owner; a removal racing a promotion ends with the member removed everywhere | T5–T8, T14, T18 |
| AC6 | A role change made just before its author leaves reaches every device | T9, T15, T22 |
| AC7 | A new owner pulls the member list from a relay; the count keeps updating after the creator leaves | T10, T11 |
| AC8 | An older profile event or link read never replaces a newer profile | T12 |
| AC9 | Background owner work (promotion, relay publication, count, removal wins, roster recovery) completes after a link server outage and after a restart, once | T16, T17 |
| AC10 | `LSWP` swaps link data and keys atomically on every store, and a restart replays both | §7 simplexmq |
| AC11 | Single-owner channels behave as today: the existing `-m "channels"` group passes | §7 |

## 2. Non-goals

Protection against current or removed owners (they are trusted as the single owner is today); retiring or rotating the root key; ordering block and unblock; relay forwarding order and queueing to backlogged relays; field-level profile merge; equal-version roster tie-break; decline, withdrawal or cancel of owner invitations; profile images in the member catch-up; older apps beyond failing closed.

## 3. Current state

| Where | Today |
|---|---|
| `Internal.hs:1669-1685` `groupLinkData` | owners = `[self]`, only with the root private key |
| `Internal.hs:1618-1631` `updatePublicGroupData` | creator republishes the count with an async `LSET` on every join or leave; throws without a group link row |
| `Commands.hs:2994-2995` | owner role refused in channels |
| `Commands.hs:3246`, `Internal.hs:1739-1748` | leaving deletes the link queue with `DEL` |
| `Internal.hs:2642-2654`, `Subscriber.hs:1431-1482` | roster version = local gate + 1, one signer; relays activated only after the creator's own async write |
| `Store/Groups.hs:2353-2373` | relays read owners once, at invitation, and throw for an owner without a row; the relay check reads only `relays` (`Commands.hs:6001-6020`) |
| `Subscriber.hs:3783-3786` `xGrpMemDel` | an admin's removal of an owner is refused, and not forwarded |
| `Subscriber.hs:4019-4020` | forwarded content from departed authors ignored |
| simplexmq `Server.hs:1574-1579`, `QueueStore/*` | `LSET` and `RKEY` are separate and last-writer-wins |
| simplexmq `Agent.hs:1337-1356`, `:2207-2262` | the link read keeps the encrypted bytes local; async commands belong to a connection, are retried by the agent and answer with the chat command's ID |

## 4. Design

The link server orders everything owners share. One new SMP command compares the hash of the stored encrypted user data and swaps the user data and, when owners change, the recipient key set in one atomic step. Every multi-owner link write reads, applies only its own change, and swaps. The link data commits the roster version and its author, so each version has one author who had applied every earlier one: members and relays keep today's roster protocol. A user's command writes synchronously and returns the result; work an owner does in the background is one link sync, made of agent async commands whose continuations re-derive what to do from the latest read and the database, so it survives restarts and outages.

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

Every owner, the creator included, writes with explicit credentials: `LinkOwnerCreds {server :: SMPServer, linkKey :: LinkKey, rcvId :: SMP.RecipientId, rootKey :: C.PublicKeyEd25519, rcvAuthKey :: SMP.RcvPrivateAuthKey, sigKey :: C.PrivateKeyEd25519}`. `rcvAuthKey` signs the SMP command and must be a recipient key of the queue: the creator's link-queue key, or another owner's member key. `sigKey` signs the user data: the root key on a device that holds it, otherwise the member key.

| Function | Does |
|---|---|
| `getConnShortLinkForWrite :: AgentClient -> NetworkRequestMode -> UserId -> ConnShortLink 'CMContact -> AE (FixedLinkData 'CMContact, ConnLinkData 'CMContact, LinkDataHash)` | the contact branch of `getConnShortLink'` (`Agent.hs:1337-1356`), proxied as `LGET` is, also returning the hash of the received user data |
| `setShortLinkSwap :: AgentClient -> NetworkRequestMode -> UserId -> LinkOwnerCreds -> LinkDataHash -> Maybe (NonEmpty RcvPublicAuthKey) -> UserContactData -> AE ()` | require `sigKey`'s public key to be `rootKey` or in `owners`, and `validateLinkOwners rootKey owners` (`Agent/Protocol.hs:2003-2017`); `encodeSignUserData` with `sigKey`, `encryptUserData` with the key from `contactShortLinkKdf linkKey`; `LSWP` signed with `rcvAuthKey` |
| `setShortLinkSwapAsync :: AgentClient -> ACorrId -> ConnId -> LinkOwnerCreds -> LinkDataHash -> Maybe (NonEmpty RcvPublicAuthKey) -> UserContactData -> AE ()` | the same as an async command on `(connId, Just server)`, `connId` being a mailbox only (as `getConnShortLinkAsync`'s, `Agent.hs:1206-1210`); event `OK` |
| `deleteLinkQueueWithCreds :: AgentClient -> NetworkRequestMode -> UserId -> LinkOwnerCreds -> AE ()` | `DEL` with `rcvAuthKey`, to destroy the channel |
| `getConnLinkQueueCreds :: AgentClient -> ConnId -> AE (SMP.RecipientId, SMP.RcvPrivateAuthKey)` | local read of the creator's link queue ID and key, as `getConnLinkPrivKey` (`Agent.hs:494-496`, `:1312-1318`) reads its link signing key |
| `linkServerSupportsSwap :: AgentClient -> NetworkRequestMode -> UserId -> SMPServer -> SMP.LinkId -> AE Bool` | connect on the link session, compare the negotiated version with v24 |
| `linkServerPrivateRouted :: AgentClient -> UserId -> SMPServer -> AM Bool` | whether the user's settings route commands to this server privately and the connection is not IP-protected (`sendOrProxySMPCommand`'s `shouldUseProxy`, `ipAddressProtected`, `Agent/Client.hs:1190-1211`) |

- **Async read with the hash.** `LDATA` gains the hash: `LDATA :: FixedLinkData 'CMContact -> ConnLinkData 'CMContact -> ConnectionRequestUri 'CMContact -> LinkDataHash -> AEvent AEConn` (`Agent/Protocol.hs:407`); `getConnShortLink'` computes it from the bytes it read (`:1339`), the sync `getConnShortLink` drops it, and the two chat matches (`Subscriber.hs:1245`) and agent tests take the extra field. `getConnShortLinkAsync` is unchanged.
- **Async swap command.** `ACommand` gains `LSWP LinkOwnerCreds LinkDataHash (Maybe (NonEmpty RcvPublicAuthKey)) UserContactData` (`Agent/Protocol.hs:466-475`), tag `"LSWP"`, `strEncode` with the user data last, because its parser consumes the rest of the input (`:2075-2079`); worker branch `withServer' . tryCommand $ setShortLinkSwap' c NRMBackground userId creds h ks_ ud >> notify OK` (as `LSET`, `Agent.hs:2255-2258`). The private keys stay in the agent's command table until the command completes. No migration: commands are stored as blobs without a version (`Agent/Store.hs:509-520`).
- **Link session.** A non-proxied read (sync or async), the swaps, `DEL` and the version check use one transport session per link, `(userId, server, Just linkId)` through `withClient`/`withLogClient` (`Agent/Client.hs:1153-1159`), not the session shared per user and server (`mkTSession`, `:1442-1444`), which would tie them to the user's other queues there. The link ID comes from the link key.
- **Direct connections.** The creds calls and `linkServerSupportsSwap` fail with `CMD PROHIBITED` when the user prohibits direct connections to unknown servers (`smpProxyFallback = SPFProhibit`) and the connection is not IP-protected.
- `Crypto/ShortLink.hs`: extract and export `mkOwnerAuth :: OwnerId -> C.PublicKeyEd25519 -> C.PrivateKeyEd25519 -> OwnerAuth`; `newOwnerAuth` (`:81-85`) uses it.
- Errors chat sees: `SMP _ CONFLICT`; `SMP _ AUTH` (key not in the set); `BROKER _ (TRANSPORT TEVersion)` (server below v24); `CMD LARGE` (user data over 13,784 bytes); `CMD PROHIBITED` (direct connection prohibited). The async worker retries temporary and host errors without limit and reports the others as `ERR` (`Agent.hs:2446-2466`).

### 4.3 Link data, owners, modes

`GroupShortLinkData` (`Protocol.hs:1621-1631`) gains optional fields; `UserContactData` is unchanged and a write carries its `direct` and `ratchetKeys` from the read.

| Field | Type | Written | Meaning |
|---|---|---|---|
| `linkVersion` | `Maybe Int64` | every channel link write: in a swap read + 1; in a single-owner write `link_data_version` + 1, persisted when the write is made (async writes: when queued) | write counter; absent means not gated |
| `revokedOwners` | `Maybe [MemberId]` | removal and leave | departed owners; never shrinks |
| `creatorQueueKey` | `Maybe CreatorQueueKey {memberId :: MemberId, key :: SMP.RcvPublicAuthKey}` | the creator, in every swap from its first promotion | its link-queue recipient key |
| `linkRcvId` | `Maybe SMP.RecipientId` | the creator, in every swap from its first promotion | queue address for other owners; authorises nothing |
| `roster` | `Maybe LinkRoster {version :: VersionRoster, author :: MemberId}` | every roster claim and the first promotion | the committed roster version and its author |

Keys, IDs and `OwnerAuth` encode in JSON as base64url of their `smpEncode` (`strToJSON`/`strParseJSON`). A write encodes the record and merges it over the read JSON object (left-biased union), keeping keys this version does not know; `GroupShortLinkData` today drops unknown fields on parse.

**Current owner** (`currentOwner m = memberRole' m == GROwner && not (memberRemoved m)`) is used for every owner check below. Owner rows created from the chain or a commit have status `GSMemUnknown` (`createLinkOwnerMember`, `Store/Groups.hs:3575-3603`), which `memberCurrent` excludes.

**Multi-owner mode** (`isMultiOwnerChannel`): this device holds an owner row other than its own membership (any status), or has an owner invitation outstanding (`owner_inv_msg_id` set on any member). From the first invitation every link write of the device goes through §4.4; single-owner channels keep `setGroupLinkData`/`setGroupLinkDataAsync` (`Internal.hs:1588-1604`) and add only `linkVersion`.

**Credentials** for a write are built each time: the channel link's server and key, `rcvId` and `rcvAuthKey` from `getConnLinkQueueCreds` on the creator and from the read's `linkRcvId` and the member key elsewhere, the group's root public key, and `sigKey` (§4.2).

### 4.4 Writes, reads, link syncs

**Locks.** `ChatController.linkWriters :: TMap GroupId Lock` serialises this device's user-command link writes per channel. Network calls never run under the group lock (`withGroupLock`), which every agent event of the channel takes on the single event thread (`Subscriber.hs:146-147`, `Store/Connections.hs:67-68`); DB steps take it briefly.

**`writeChannelLink :: User -> GroupId -> ChannelLinkChange -> (LinkWrite -> ChannelLink -> CM a) -> CM a`** (`LinkWrite = LWWritten | LWUnchanged`), for user commands, under the link writer lock:

1. Read with `getConnShortLinkForWrite` on `publicGroup.groupLink`; decode `UserContactData` and `GroupShortLinkData` with its raw JSON; `channelLinkWriteHook` (tests).
2. Under the group lock: `applyChannelLinkData` (below), then `applyLinkChange :: ChannelLinkChange -> ChannelLink -> CM (Either ChannelLinkError (Maybe ChannelLink))`; `Right Nothing` = already applied, no write. If the read shows this device departed: stop with `CLENotOwner` (a leave instead completes its departure, §4.9).
3. When the owner set changed, `linkQueueKeys`: `creatorQueueKey.key` unless the creator is revoked, plus the `ownerKey` of every other chain entry not in `revokedOwners`; the last-owner check keeps it non-empty.
4. `linkVersion` = read (0 if absent) + 1; `setShortLinkSwap` with the read's hash.
5. `CONFLICT`, a network error or a timeout → from step 1 (a lost response is found applied, step 2), at most `channelLinkWriteAttempts` (5), then `CLELinkBusy`. `AUTH` → one more read and apply (it shows the revocation), `CLENotOwner`. `TEVersion` → `CLELinkServerVersion`. `CMD LARGE` → `CLELinkDataTooLarge`. `CMD PROHIBITED` → `CLEDirectConnection`.
6. On success (`LWWritten`), and on "already applied" (`LWUnchanged`), under the group lock: store the link's `linkVersion` in `link_data_version` and its roster commitment in `link_roster_version` (as `applyChannelLinkData` step 5), and run the callback: local state, events to send, the result.
7. Release the writer lock; enqueue a link sync if the read left background work pending (below).

Callers never hold the group lock across this call. Commands that hold `withGroupLock` across their body today and must take it only around their DB steps: `APIMembersRole` (`Commands.hs:2984`), `APIRemoveMembers` (`:3123`), `APILeaveGroup` (`:3236`), `APIDeleteChat` (`:1390`), `APIAddGroupRelays` (`:2781`).

**Link sync**, the background path. `enqueueLinkSync :: User -> GroupInfo -> LinkSyncKind -> CM ()` (`LSSync | LSResign`, §4.10) runs in the main app only (§4.15). Its mailbox is the owner's connection to the current relay with the lowest member ID whose connection is `ConnReady`, which an `ERR` does not mark failed (`agentMsgConnStatus`, `Subscriber.hs:178`), and whose events run under the group lock; without one there is no sync until the next trigger or chat start. New `CommandFunction`s (`Types.hs:2162-2225`): `CFChannelLinkSync`, `CFChannelLinkResign` (expect `LDATA_`) and `CFChannelLinkSwap` (expects `OK_`).

1. `createCommand db user (Just mailbox) fn`, then `getConnShortLinkAsync` with the mailbox (`Internal.hs:3160-3165`).
2. `LDATA` continuation (`processGroupMessage`, `Subscriber.hs:1245`, under the group lock): decode; `channelLinkWriteHook`; `applyChannelLinkData`; if this device is departed, stop. Run the callbacks the read shows due, then apply each pending change to the read in turn (`applyLinkChange`; a refusal is handled per change and the others go on). If the result differs from the read: key set and `linkVersion` as in steps 3–4, `createCommand … CFChannelLinkSwap`, `setShortLinkSwapAsync` with the read's hash; else stop.
3. Swap continuation: on `OK`, and on `ERR` with `CONFLICT` or `AUTH` (`Subscriber.hs:1234-1236`, `:1316-1320`, now dispatched on the function), enqueue an `LSSync`; the next read runs the callbacks. The agent retries network errors itself, and a landed swap it retries gets `CONFLICT`. Any other `ERR`: an event to the view, stop.

Nothing is carried between the steps but the command row: every decision comes from the latest read and the database, so a lost or repeated event (an agent event can be lost or repeated across a crash, `Agent.hs:2459`, `:2467-2470`) and a crash between steps are absorbed by the next trigger or by chat start.

**Changes**, with when a sync finds each pending and the callback it runs once a read shows it applied:

| `ChannelLinkChange` | Applies | No write / refuses | In a sync: pending when → callback |
|---|---|---|---|
| `CLCProfile base new` | the profile, whole | read = `new` → no write (landed); read ≠ `base` → `CLEChangedByOwner` | user commands only |
| `CLCAddOwner member ownerAuth` | entry after the signer's; key into the set; first promotion also `creatorQueueKey`, `linkRcvId`, `roster` (§4.6) | member revoked or departed → refuse; already in the chain → no write | a member has `owner_inv_accepted = 1` → §4.6 step 4 |
| `CLCRevokeOwner memberId` | memberId into `revokedOwners`; its key out | already revoked → no write; no other current owner would remain → `CLELastOwner` | a member held departed is a non-revoked chain entry → none (§4.8) |
| `CLCRelays added removed` | read relays + added − removed − relays held as left | no change → no write | an acknowledged relay (`RSAcknowledgedRoster`) is missing, or a relay held as left is listed → `activateGroupRelays` (§4.13) |
| `CLCCount n` | `publicGroupData.publicMemberCount = n` | not the first current chain entry, or read count = `n` → no write | `members_catchup = 0` and the tally differs from the read count → none |
| `CLCRoster claim` | `roster` (§4.10) | §4.10 | §4.10 rules (b), (c) → rule (a) |

Triggers: an acceptance (§4.6), a commit or read showing a departed member as a chain owner, an owner applying the removal or leave of a member it held as an owner (§4.8), a relay's acknowledgement or its own leave (§4.13), the count timer (§4.11), a relay connection becoming ready or resuming and relay set-up (§4.10, `LSResign`), a user write or channel open whose read shows pending work, and chat start (§4.15).

Chain rules: signing order is kept (`validateLinkOwners`); the creator signs new entries with the root key (`Commands.hs:2757`), others with their member key (`mkOwnerAuth`); a revoked entry is dropped once no current entry depends on it, transitively, except the writer's own entry in its leave write (a non-root writer must be in the chain, `Agent/Protocol.hs:1996-2001`). An entry is about 124 bytes of the 13,784-byte user data.

**`applyChannelLinkData`**, under the group lock, on every channel link read: members at channel open (`APIGetUpdatedGroupLinkData`, `Commands.hs:1959-1972`) and connect plan (`:4624`, `:4651`); owners at channel open, in every link sync and in step 2 of every write; relays at invitation (`getLinkDataCreateRelayLink`, `Subscriber.hs:4585-4606`) and in every relay check (`checkRelayServedGroups`, `Commands.hs:6001-6020`); joiners in `APIConnectPreparedGroup` (`Commands.hs:2303-2333`).

1. A read whose `linkVersion` is below `link_data_version` is ignored; else store it.
2. Profile and count as `updateGroupFromLinkData` (`Internal.hs:1633-1654`).
3. `revokedOwners`: each held member becomes departed (`GSMemLeft`) at whatever role it is held; unknown memberIds get departed rows. A relay keeps a departed owner's connection (§4.13). Own memberId listed while the membership is current: complete the departure (§4.9 step 3).
4. Non-revoked chain entries: a member held neither as owner nor departed becomes owner with `ownerKey` (created with a placeholder profile if unknown); one held as departed: on an owner, a link sync (§4.8); its own entry with its own key and a current membership: become owner (§4.6); with another key: alert item, stay a subscriber. A read only adds owners and marks revoked ones departed, so one older than a promotion cannot undo it.
5. `roster` → `link_roster_version`; `link_roster_seen_at` = now when the version changes.
6. Owners: join each listed relay without a relay member (`connectToRelayAsync`, `Internal.hs:1606-1616`); `RSInactive` for active `group_relays` rows missing from the list (as `Subscriber.hs:1466-1477`). When the read `roster.version` is above `applied_complete_roster_version`, `XGrpRosterRequest` to every current relay (§4.10).

Every channel owner reads on channel open (single-owner creators too, which refreshes `link_data_version` after a restore).

### 4.5 Events and wire

`multiOwnerVersion = VersionChat 23`; `currentChatVersion` 22 → 23 (`Protocol.hs:96-97`).

| Event | Payload | Path | Signed | Forwarded |
|---|---|---|---|---|
| `x.grp.owner.inv` | `{memberId}` | owner → relays → invitee's support scope | yes | `DJSMemberSupport` of the invitee |
| `x.grp.owner.acpt` | `{invId :: SharedMsgId}` | invitee → relays → its support scope | yes | `DJSMemberSupport` of the sender |
| `x.grp.owner.add` | `{memberId, memberKey :: MemberKey, profile :: Profile}` | owner → relays → all | yes | `DJSGroup` |
| `x.grp.members.req` | `{cursor :: Maybe MemberId}` | owner → one relay | no | no |
| `x.grp.members.page` | `{members :: [{memberInfo :: MemberInfo, memberRestrictions :: Maybe MemberRestrictions}], cursor :: Maybe MemberId}` | relay → owner | no | no |
| `x.grp.info` (changed) | adds optional `linkVersion` | as today | yes | yes |

Each event: constructor, `CMEventTag`, tag string and parser, `toCMEventTag`, JSON params (`Protocol.hs:460-1557`), `requiresSignature` (`:1357-1371`), `isForwardedGroupMsg` (`:528-549`), relay dispatch (`Subscriber.hs:1080-1132`), forwarded dispatch (`:4041-4064`), and the Bot API docs (`bots/src/API/Docs/{Commands,Responses,Events,Types}.hs`, with the generated `bots/api/*.md`, TypeScript and Python types, checked by `tests/APIDocs.hs`). Older clients ignore them.

### 4.6 Promotion with consent

**Invite.** `APIInviteGroupOwner {groupId, groupMemberId}` (CLI `/_owner invite #<groupId> <groupMemberId>`, `/owner invite #<group> <member>`) → `CROwnerInvitationSent {user, groupInfo, member}`. Refused unless:

- the user is a current owner of a channel; M is a current member with a key, not an owner, not departed;
- M's code is verified: `memberVerifiedCode` equals `getChannelMemberCode` now (`Commands.hs:3811-3815`), else `CLENotVerified` (a relay that substituted M's key would sign the acceptance with it; only the out-of-band check detects it);
- every current relay supports `multiOwnerVersion`, else `CLERelayVersion`;
- `linkServerSupportsSwap`, else `CLELinkServerVersion` (`CMD PROHIBITED` → `CLEDirectConnection {host, prohibited = True}`).

It sends `x.grp.owner.inv {memberId}` and stores the message's shared msg id in M's `owner_inv_msg_id` (a new invitation replaces it), which puts the device in multi-owner mode. M's version is not checked (known only from M's join, `Store/Groups.hs:2533-2551`); an older M ignores the invitation.

**Receivers.** Relay: sender a current owner, M current → forward to M's support scope. M: author a current owner and the memberId its own → item `RGEOwnerInvitation {invStatus = OISPending}` in its own support scope (`mkGetMessageChatScope`, `Internal.hs:1984-2003`, which also sets `supportChat`). Others: no item.

**Accept.** `APIAcceptGroupOwnerInvitation {groupId, chatItemId, allowDirect :: AllowDirect}` (`AllowDirect = ADAsk | ADYes`; CLI `/_owner accept #<groupId> <chatItemId>[ direct=on]`) on M: the item must be a pending invitation, the membership current, not owner. Direct connections prohibited and the connection not IP-protected: `CLEDirectConnection {host, prohibited = True}`, whatever `allowDirect` says. `ADAsk` and `linkServerPrivateRouted` for the link's server: `CLEDirectConnection {host, prohibited = False}`; the app asks and repeats with `ADYes`. It sends `x.grp.owner.acpt {invId}` (the item's shared msg id), sets the item to `OISAccepted`, returns `CRChatItemUpdated`. Relays forward it to M's support scope; the promoter acts when `invId` equals M's `owner_inv_msg_id`.

**Promote.** On the acceptance the promoter sets M's `owner_inv_accepted = 1` and enqueues a link sync. In the sync (§4.4):

1. Re-check M (current, not departed or revoked) and the relays' versions; a failure that persists clears the invitation and is shown to the user.
2. First promotion (the creator, a read without `creatorQueueKey`): if `roster_version` is set, set `applied_complete_roster_version` to it, rebuild the blob from current roster members without M (`buildGroupRoster`) and store it as its own (§4.10), and commit `roster {version, self}`; with no roster version, commit none (the first claim takes version 0). The change also writes `creatorQueueKey` and `linkRcvId` (`getConnLinkQueueCreds`).
3. `CLCAddOwner M (mkOwnerAuth …)`.
4. Callback, when a read shows M's entry with M's key while M's `owner_inv_accepted = 1` (a repeated read finds it cleared and sends nothing): clear `owner_inv_msg_id` and `owner_inv_accepted`; M's role owner (its key is the chain key), `owner_since` = now; send `x.grp.owner.add {memberId, memberKey, profile}` with M's redacted profile (`redactedMemberProfile`, `Internal.hs:1361`); item `SGEMemberRole … GROwner`; `CEvtMemberRole`.

**Commit.** Relays, members and owners apply `x.grp.owner.add` when the sender is a current owner and `memberId` is not held as departed: role owner, `member_pub_key = memberKey` (warning if it replaces a different key), the member created from `profile` if unknown, `owner_since` = now, the role item as `xGrpMemRole` creates it. A relay sends its capabilities (`XGrpRelayCap`) to the new owner. An owner holding `memberId` as departed enqueues a link sync (§4.8). It is not `x.grp.mem.role … owner`, so older clients ignore it and no relay-asserted key is promoted (`Subscriber.hs:3465-3468`).

**M becomes owner** when it applies the commit about itself or reads its own entry (§4.4), with its own key and a current membership: role owner, `owner_since` = now, item `RGEUserRole GROwner`, `members_catchup = 1`, member catch-up (§4.7). It stores no credential: each write builds them.

### 4.7 New owner's member list

- **Request.** `x.grp.members.req {cursor}` to one current relay with a ready connection; `cursor = Nothing` starts from the beginning. A watchdog (`ChatController.memberCatchups :: TMap GroupId (Int, Maybe MemberId)`, a request counter and the cursor) re-requests from the same cursor at the next ready relay when no page arrived within `memberCatchupTimeout` (1 minute). It is a forked delay, not a network step: the request itself is an ordinary message. Main-app chat start restarts from the beginning while `members_catchup = 1`.
- **Relay.** Serves a current owner only (silence otherwise; the watchdog moves on). One page: current members (statuses of `getGroupMembersByCursor`, `Store/Delivery.hs:352-390`) with `member_id > cursor ORDER BY member_id` (memberIds are the same on every relay and indexed, `UNIQUE(group_id, member_id)`; the cursor member's own row may be gone), excluding relays and the requester; `memberInfo` with the redacted profile and no image, plus restrictions, as many as fit one message (`maxEncodedMsgLength`, `Protocol.hs:929`); `cursor` = the last packed member's memberId, absent when none follows.
- **Owner.** A page from a current relay while `members_catchup = 1`, in one transaction: a held `GSMemUnknown` member gets its profile and status as `xGrpMemNew` gives them (`updateRosterMemberAnnounced`, `Subscriber.hs:3243-3251`), never role or key; departed and other held members are skipped; new ones are created by `xGrpMemIntro`'s channel rule (`Subscriber.hs:3326-3334`, extracted): roles from member up at the default role without a key (the roster establishes those), subscribers with the relay-reported key, as owners keep it from `XGrpMemNew` today. No per-member link write. Then the next request, or `members_catchup = 0`. A page from a relay asked earlier is applied, but only the relay asked last gets the next request. Without images 40–60 members fit a message.

### 4.8 Removing an owner

- **Command.** `APIRemoveMembers` with a current owner as the only target, by a current owner, in a channel. `writeChannelLink` with `CLCRevokeOwner X`; callback: mark X removed (rows kept, §4.14) and send `x.grp.mem.del X False Nothing` (`deleteMemsSend`, `Commands.hs:3191-3229`). Removing oneself is a leave. A crash between the swap and the callback sends no announcement; parties learn from link reads, relays within their check interval.
- **Receivers.** Relays and members apply an owner's removal of an owner from a current owner (`checkRole`, `Subscriber.hs:3783-3786`), as today; a relay forwards it to X first, then deletes X's connection (`:3755-3761`).
- **The removed owner's device** applies the signed removal of itself (own-membership branch, `Subscriber.hs:3734-3746`) on receipt, and a read showing itself revoked has the same effect (§4.4 step 3): it completes its departure: membership removed, keys erased (§4.9 step 3), catch-up stopped.
- **A removal racing a promotion.** A removal of a member and its promotion reach parties in either order. A party that applied the removal refuses the commit (§4.6); one that applied the commit applies an owner's removal of an owner as usual, and applies an admin's removal of a member it made an owner less than `promotionRaceWindow` (1 day) ago (`owner_since`) as it would an owner's (`checkRole` gains this case in both branches of `xGrpMemDel`): relays, members, owners and the member's own device. Older owners, and owners learned from the chain (no `owner_since`), are refused as today. Every party ends with the member removed.
- **Removal wins.** An owner that holds member M as departed (left or removed) and sees M as a non-revoked chain entry (a commit or a read), or that applies the removal or leave of a member it held as an owner, enqueues a link sync; the sync revokes M when the read still lists it. It sends no announcement: the removal itself reached every party.
- Mutual removal: the first swap removes the other's key; the second gets `AUTH`, or `CONFLICT` and a read showing itself revoked.

### 4.9 Leaving

`APILeaveGroup` by an owner in multi-owner mode (single-owner channels keep `Commands.hs:3233-3277`):

1. Read. Refuse with `CLELastOwner` if no other current owner. If `roster.author` is this owner and either `stored_roster_version` is below `roster.version` (its claim landed but was not stored) or its stored roster is undelivered (no relay has `SENT` the last chunk of its `x.grp.roster` at `stored_roster_version`, in `msg_deliveries`, or there is no such message; in channels a message to a relay that is not ready is dropped, `memberSendAction`, `Internal.hs:2840-2846`), make an `RCResign` claim first (§4.10, through `writeChannelLink`; it sends the blob and one role event) and wait up to `channelOwnerWaitTimeout` (30 s), without the group lock (`SENT` is processed under it), for `SENT` of the new blob's last chunk to a relay; else refuse with `CLERelaysUnreachable`.
2. `writeChannelLink` with `CLCRevokeOwner self` (own entry kept, key dropped); the last-owner check runs on every attempt.
3. Complete the departure, only while the membership is current: `x.grp.leave` to relays; delete member connections waiting for delivery; membership `GSMemLeft`; erase `member_priv_key` (a later load generates an unrelated key, `Store/Shared.hs:750-762`); the creator replaces `root_priv_key` with the public key and deletes its group link (`deleteGroupLinkIfExists`: its `DEL` gets `AUTH`, and the agent deletes the queue record locally, `Agent.hs:3080-3088`; no later write restores the key).

A crash after step 2 is completed by the next read (§4.4 step 3). A repeated leave whose read shows itself already revoked completes step 3 and returns `CRLeftMemberUser`.

### 4.10 Roster ordered by the link

**Claim.** In multi-owner mode `APIMembersRole` and `APIRemoveMembers` call `claimRosterVersion` instead of `broadcastRoster` (`Commands.hs:3005-3008`, `:3136-3139`): `writeChannelLink` with `CLCRoster (RCChange delta targets startRoles)`, where `startRoles` are the targets' roles loaded when the command started. `RosterClaim = RCChange RosterDelta [GroupMember] [GroupMemberRole] | RCResign`. With `r` = read `roster.version` and `f` = `applied_complete_roster_version` (none = −1); a read without `roster` (before the creator's first promotion lands, or in a channel that never had a roster) gives `r` = `f` = this device's `roster_version` (none = −1), so a claim never goes below members' gate:

1. If `r > f`: if the read author is this owner (its claim `r` was lost before it was stored or sent), or the author is departed and the abandonment rule holds (below), claim from its own state; otherwise send `XGrpRosterRequest` with its `stored_roster_version` to every current relay and refuse with `CLERosterSyncing`.
2. `RCChange`: any target role ≠ its start role → `CLEChangedByOwner`.
3. Build the blob at `r + 1` from current roster members with the delta (`buildGroupRoster (applyRosterDelta …)`, `Internal.hs:2629-2654`); `roster = {r + 1, self}`.
4. Callback, only on `LWWritten` (an unchanged link claimed nothing; return `Nothing`): `setOwnGroupRoster` sets `roster_version`, `applied_complete_roster_version`, `stored_roster_version` to `r + 1`, `roster_blob`, sender = own membership (header columns NULL); `sendStoredRoster`; return `r + 1` for the events.

Single-owner channels keep `broadcastRoster`. Owners outside the roster: `processRosterEntries` (`Subscriber.hs:3610-3630`) skips current owners, the user's own membership included, and departed members; `xGrpMemRole` ignores owner targets and the owner role; `APIMembersRole` refuses owner targets and the owner role in channels.

**Stored blob.** `getOwnStoredRoster` returns `(stored_roster_version, roster_blob)`; `sendStoredRoster` sends `XGrpRoster {version, sha512 of the bytes}` and the chunks of those exact bytes (`sendRoster`'s steps without re-encoding, `Internal.hs:2665-2681`), and records the version in each relay member's `roster_served_version` (unused on owners today). In multi-owner mode a relay gets a roster only this way and only when `stored_roster_version` equals `link_roster_version`, the commitment last read or written (an owner's complete frontier can advance from another owner's events without the blob, so it is not the test); otherwise the owner requests the roster with its `stored_roster_version` (relays serve when theirs is greater) and sends after completion. This covers relay set-up (`Subscriber.hs:937-942`: in multi-owner mode it creates no version; with no roster in the link it enqueues an `LSResign` sync, which claims the next version), the acknowledgement (`xGrpRosterAck`, `:3654-3672`: compared with that relay's `roster_served_version`; a lower one re-sends once per newer stored version) and every re-send.

**Departed authors.** Owners and members process a forwarded `XGrpRoster` header and versioned `XGrpMemRole` and `XGrpMemDel` from a departed owner by the roster gate, as from a current one (`xGrpMsgForward`, `Subscriber.hs:4019-4020`; roster chunks do not pass there, `:1131`); other content stays ignored. A relay processes them on a departed owner's kept connection up to `link_roster_version` (§4.13). Relays answer a current owner's `XGrpRosterRequest` whenever stored > requested (`xGrpRosterRequest`, `:3679-3685`), without the served-version bound kept for members.

**Link sync rules**, the background roster work:

- (a) In every sync, when the read author is this owner with `stored_roster_version` below the read version, and no user write of this device holds the link writer lock (a held lock means a claim between its swap and its callback): store its own roster at that version (`setOwnGroupRoster`), `sendStoredRoster`, and one `x.grp.mem.role` at that version: the current role of the first current non-owner, non-relay member with a key, by memberId. Any event at that version makes members that missed earlier ones see a gap and fetch the blob (`applyAtRosterVersion` acts before the target lookup, `Subscriber.hs:3410-3424`).
- (b) In every sync, when `r > f`, the author is departed and the abandonment rule holds: an `RCResign` claim from its own state.
- (c) In `LSResign` syncs only, when the read author is this owner at its stored version and its stored roster is undelivered (as §4.9 step 1), or the read has no roster and a relay waits for its first one: an `RCResign` claim.

An `RCResign` claim writes `roster {r + 1, self}` with no delta; in a sync rule (a) then sends it, and through `writeChannelLink` its callback does the same. A sync never sends an event at a version it is re-sending.

**Abandonment.** A version `r` by a departed author, with `f < r`, is abandoned (claim `r + 1` from its own state; nobody received `r`) when `link_roster_seen_at` is older than `rosterAbandonTTL` (1 day) and a request this owner sent to every current relay at least `rosterRequestTimeout` (5 minutes) earlier brought no blob at `r`: `ChatController.rosterRequests :: TMap GroupId UTCTime` keeps the time of the first unanswered request (set when a read or claim sends one while behind, cleared when `f` reaches the read version). Claims and syncs check the rule when they need it; there is no timer. An owner that also missed earlier versions of that author (`f < r − 1`) drops them too, though some devices received them; otherwise roles could never change again. A current author's version is never abandoned: its device sends it when back (rules (a), (c)), and a lost device is removed first.

**Undelivered roster.** At main-app chat start, when a relay connection becomes ready (`CON`, `Subscriber.hs:889`) and when it resumes after a full queue (`continueSending` true on `SENT` or `QCONT`, `:1197-1199`, `:1291-1295`): an owner whose stored roster is at `link_roster_version`, was authored by itself and is undelivered enqueues an `LSResign` sync.

### 4.11 Subscriber count

In multi-owner mode the owner branch of `updatePublicGroupData` (`Internal.hs:1618-1631`) recounts (`updatePublicMemberCount`) and, when `members_catchup = 0` in the main app, schedules a publish: the first trigger inserts an entry in `ChatController.countPublishPending :: TMap GroupId ()` and forks a thread that sleeps `channelCountPublishInterval` (60 s), deletes it and enqueues a link sync (which writes the count only on the first current chain entry). The fork is a delay bounding count writes, which would make other owners' swaps conflict during a burst of joins; the agent has no delayed commands. The relay-announcement threshold in relay activation becomes `publicMemberCount > number of current owners` (`Subscriber.hs:1445-1449`).

### 4.12 Profile edits and the link counter

- In multi-owner mode `runUpdateGroupProfile` (`Commands.hs:4164-4192`, behind `APIUpdateGroupProfile`, `APISetPublicGroupAccess` and the other profile commands) calls `writeChannelLink` with `CLCProfile base new`, where `base` is the local profile when the command starts; `CLCProfile` compares profiles as `J.toJSON` values. The callback writes the local profile, then `XGrpInfo` goes out with the written `linkVersion`. The command API is unchanged.
- The apps' profile editors refuse to save, with the "changed by another owner" alert, when the model's channel profile changed while the editor was open (§6.1; an `XGrpInfo` from another owner updates the model). The core check covers a change that reached the link but not this device.
- `xGrpInfo` (`Subscriber.hs:3840-3865`), on relays and members alike: an event with `linkVersion` below `link_data_version` is not applied; a higher one is applied and stored. Relays forward it as today.

### 4.13 Relays in a multi-owner channel

- **Owner events** are handled as today, plus the commit (§4.6) and the admin-removal window (§4.8). Relays read the link at invitation and in every relay check and apply it (§4.4).
- **Departed owners.** A relay that learns of an owner's departure from a read keeps its connection until that owner's `x.grp.leave` or a remover's `x.grp.mem.del`, because its last blob can be queued on it. From that connection it processes only `x.grp.leave`, roster transfers (`xGrpRoster`, `:3503-3540`, and the chunks of a transfer started from it, `bFileChunkGroup`, `:2626-2633`) and versioned role and removal events, at versions up to `link_roster_version`, which the read that taught the departure set at or above the owner's last claim; the relay direct path has no other sender-status check (`Store/Connections.hs:110-112`).
- **Activation.** The `LINK` handler body (`Subscriber.hs:1437-1477`) becomes `activateGroupRelays :: User -> GroupInfoKeys -> [ShortLinkContact] -> CM ()`, called by the handler and by the `CLCRelays` callback; it reads `group_relays` and the group link row only when present. `CEvtGroupLinkDataUpdated` and `CRGroupRelaysAdded` carry `Maybe GroupLink` (`Controller.hs:862`, `:1035`).
- **Publication** after a relay's acknowledgement (`Subscriber.hs:3661-3665`) and a pre-roster relay's connection (`:946-947`): a link sync (§4.4).
- **Removal** of a relay (`Commands.hs:3149-3153`): `writeChannelLink` with `CLCRelays [] [link]`; a relay's own leave seen by any owner: a link sync.
- **`APIAddGroupRelays`** without a group link row takes the link from `publicGroup.groupLink` (`Commands.hs:2790-2793`). A relay below `multiOwnerVersion` added in multi-owner mode stays invited (its `updateRelayGroupKeys` fails); the owner removes it.
- **Owners join relays added by others** from `XGrpRelayNew` (`Subscriber.hs:3881-3886`) and on reads. Relay side: `memberJoinRequestViaRelay` (`:1690-1720`) accepts a key-proven join onto a current owner row, as for roster rows, and refuses joins onto departed rows. Owner side: `updatePreparedUserAndHostMembers'` (`Store/Groups.hs:821-844`) and the host `CON` branch (`Subscriber.hs:892-922`) leave the membership and group profile and the joined items alone when the membership is an owner.
- **Invitation by any owner.** `updateRelayGroupKeys` (`Store/Groups.hs:2353-2373`) creates an owner row (`createLinkOwnerMember`) for every chain owner without one and applies `revokedOwners`; the inviting-owner check (`Subscriber.hs:4598`) excludes `revokedOwners`.
- **Capabilities.** `XGrpRelayCap` (`Subscriber.hs:1103-1106`) is stored whatever the receiver's role (it can arrive before the commit); without this device's `group_relays` row, in `group_members.relay_web_domain` (`relayWebDomain` in `GroupMember` JSON; `updateRelayCapabilities`, `Store/Groups.hs:1901-1911`, updates only `group_relays` today). A relay sends it to an owner when it applies its commit or its join (`sendRelayCapIfNeeded`, `Internal.hs:2686-2696`, sends only on a domain change).

### 4.14 Other guards

- **Durable rows.** In channels `getGroupMembersForExpiration` and `getRemovedMembersToCleanup` (`Store/Groups.hs:1357-1382`) skip owner and roster-role rows; an intro (`xGrpMemIntro`, `Subscriber.hs:3320-3322`) never changes a departed member's status.
- **Group-link commands** `APICreateGroupLink`, `APIAddGroupShortLink`, `APIDeleteGroupLink` refused for channels (`Commands.hs:3349-3387`).
- **`APIShareChatMsgContent`** signs with any current owner's member key (`Commands.hs:1238-1240`).
- **Connect plan and join** exclude `revokedOwners` from `glOwners`, `verifyLinkOwner` and owner binding (`Commands.hs:4615-4653`, `:4806-4815`); `APIConnectPreparedGroup` creates owners only for non-revoked entries and departed rows for revoked ones (`:2324-2332`).
- **History for joiners.** A signed as-channel item whose author is no longer a current owner is served unsigned (`FwdChannel`, `itemForwardMsgs`, `Internal.hs:1540-1548`).
- **Destroying.** In multi-owner mode `APIDeleteChat` by an owner (`Commands.hs:1384-1401`) first reads and applies the link; if the read fails it refuses, for the creator as for promoted owners; a departed device completes its departure and sends no `x.grp.del`. A promoted owner then deletes the link queue with `deleteLinkQueueWithCreds` and, if that fails, returns the error and deletes nothing; the creator keeps `deleteGroupLinkIfExists` (asynchronous; current: `DEL` deletes the queue).

### 4.15 Main app and chat start

`ChatController.mainApp :: TVar Bool`, set by `startChatController` (`Commands.hs:232-269`). Link syncs, count timers and catch-up run only in the main app: the iOS notification extension starts with `mainApp = False` and does not resume agent commands (`subscribeUsers`, `Commands.hs:255`, `:336`); an event it processes leaves its trigger in a row (`owner_inv_accepted`, relay status, member status). Main-app chat start (`startChannelOwnerWork`, next to `startRelayChecks`) and `APIActivateChat` with `restoreChat` (`Commands.hs:599-610`; how iOS returns to the foreground) enqueue an `LSResign` sync for each multi-owner channel where the user is a current owner, which covers every pending kind of §4.4, and restart a pending member catch-up.

### 4.16 Types, database, configuration

| Kind | Name | Notes |
|---|---|---|
| command | `APIInviteGroupOwner {groupId, groupMemberId}` | → `CROwnerInvitationSent`; record syntax and Bot API docs, as every command |
| command | `APIAcceptGroupOwnerInvitation {groupId, chatItemId, allowDirect}` | → `CRChatItemUpdated` |
| error | `CEChannelLink {channelLinkError :: ChannelLinkError}` | `CLENotVerified`, `CLERelayVersion`, `CLELinkServerVersion`, `CLEDirectConnection {host, prohibited}`, `CLEChangedByOwner`, `CLERosterSyncing`, `CLELastOwner`, `CLENotOwner`, `CLELinkDataTooLarge`, `CLELinkBusy`, `CLERelaysUnreachable` |
| item | `RGEOwnerInvitation {invStatus :: OwnerInvitationStatus}` | `OISPending`, `OISAccepted` |
| command function | `CFChannelLinkSync`, `CFChannelLinkResign`, `CFChannelLinkSwap` | text `channel_link_sync`, `channel_link_resign`, `channel_link_swap` |
| column | `groups.link_data_version INTEGER` | highest `linkVersion` read, written, or carried by an applied `x.grp.info` |
| column | `groups.link_roster_version INTEGER`, `link_roster_seen_at TEXT` (`TIMESTAMPTZ`) | last read roster commitment; when its version first appeared |
| column | `groups.members_catchup INTEGER NOT NULL DEFAULT 0` | new owner's catch-up pending |
| column | `group_members.owner_inv_msg_id BLOB` (`BYTEA`), `owner_inv_accepted INTEGER NOT NULL DEFAULT 0` | promoter: the pending invitation to this member; accepted, promotion to run |
| column | `group_members.owner_since TEXT` (`TIMESTAMPTZ`) | when this device made the member an owner from a commit or its own promotion |
| column | `group_members.relay_web_domain TEXT` | relay capability for an owner without the relay's row |
| config | `channelLinkWriteAttempts = 5`, `channelCountPublishInterval = 60 s`, `rosterAbandonTTL = 1 day`, `rosterRequestTimeout = 5 min`, `memberCatchupTimeout = 1 min`, `channelOwnerWaitTimeout = 30 s`, `promotionRaceWindow = 1 day` | `ChatConfig` |
| hook | `channelLinkWriteHook :: Maybe (GroupInfo -> IO ())` | `ChatHooks`, between a link read and its apply: in a user write and at channel open outside the group lock, in a link sync on the event thread; tests |
| state | `linkWriters :: TMap GroupId Lock`, `countPublishPending :: TMap GroupId ()`, `memberCatchups :: TMap GroupId (Int, Maybe MemberId)`, `rosterRequests :: TMap GroupId UTCTime`, `mainApp :: TVar Bool` | `ChatController` |

One migration `M20261015_channel_multi_owner` with `down_m20261015_channel_multi_owner`, SQLite and Postgres, registered in both `Migrations.hs` and `simplex-chat.cabal`; schema dumps regenerate from tests.

### 4.17 Races and where each is ordered

| Race | Ordered by | Where | Outcome |
|---|---|---|---|
| Two link writes of any kind | the swap | `swapQueueLinkData` | the second re-reads and re-applies, or is refused |
| A promotion by O1 while O2 removes O1 | the swap (keys and data together) | §4.4 | removal first: O1 gets `AUTH` or `CONFLICT` and reads itself revoked; else the removal keeps M's key |
| Two owners remove each other; a removal and a leave; the last two owners leaving | the swap; last-owner check per attempt | §4.4, §4.8 | one current owner remains |
| Two role changes | `roster.version` | `claimRosterVersion` | the second catches up and claims `v + 2`, or is refused for the same target |
| A role change just before leaving | the claim; the leaver's re-sign and wait for `SENT` | §4.9, §4.10 | relays, owners and members accept the departed author's roster messages |
| A role change in flight when its author is removed | relays' order of the two | §4.10 | a relay that processed the blob serves it; otherwise abandoned after a day, received by nobody |
| Two profile edits | the editor's check and the swap; `linkVersion` | §4.12 | the later is refused if the profile changed since its editor opened; devices keep the newer |
| A relay added while another is removed | the swap | `CLCRelays` | both kept |
| A member removed, by an owner or by an admin, or leaving, while promoted | the removal reaching every party; removal wins | §4.8 | the member ends departed |
| A user's role change and a background sync of the same device | the link writer lock | §4.10 rule (a) | the sync does not take the claim for a lost one |
| A read older than a promotion applied after its commit | reads only add owners | §4.4 | M stays an owner |
| A single-owner write queued before the first promotion | `LSET` refused on a multi-key queue | §4.1 | the late write gets `AUTH` |

A crash or a lost agent event in a link sync, and a party that was offline, converge at the next read (§4.4).

## 5. Compatibility and security

- **Wire.** SMP v24 (`LSWP`, `CONFLICT`, `LSET` refused on multi-key queues); a v23 server gets no `LSWP`. Chat v23 events (§4.5); `XGrpInfo` gains an optional field. Two new API commands, one new response and new types, documented in the Bot API docs. The agent's `LDATA` event gains a field and its command store a constructor (both internal).
- **Database.** One chat migration, columns only, both backends; existing rows get NULL or 0. No agent or server schema change.
- **Downgrades.** An older server skips `LKEYS` lines and restores keys from the `KEYS` lines; it loses those swaps' data since its last start and accepts `LSET` again (multi-owner writes stop: there is no `LSWP`). An older creator's `LSET` on the multi-key queue gets `AUTH`.
- **Older parties.** Promotion needs every relay at chat v23 and the link server at SMP v24 (link servers, presets included, upgrade first). Older members ignore `x.grp.owner.add` and reject the new owner's signed messages until they upgrade; older joiners take every chain entry as an owner, including revoked ones, whose keys authorise nothing on the link queue.
- **Adversary.**
  - Relay: cannot forge owner-signed events, commits, acceptances or removals; can withhold or reorder as today (link reads and the roster gap request converge); can replay old signed events, which the roster gate, the `linkVersion` gate and departed rows stop; cannot substitute a promoted member's key (verified code); can fabricate subscribers in a page, never privileged ones.
  - Link server: cannot forge link data; a stale read is ignored by `linkVersion` and can only add owners or mark revoked ones departed; ignoring the compare-and-swap brings back lost writes and roster divergence, no forgery.
  - Owners and removed owners are trusted (non-goal). Honest software sends an owner action only after a successful link write or a current read. Until a relay learns of a removal (the remover's announcement, or its link check every 15 minutes), a removed device running modified software can act there as an owner, including role events at inflated versions that stop later role changes there; afterwards the relay accepts from it only its departure and roster messages up to the link's version. With a colluding link server it can serve forged link data to readers.
  - Any current owner can destroy the channel or replace the key set with `RKEY`. An admin can remove an owner within `promotionRaceWindow` of a device learning its promotion (§4.8).
- **Privacy.** A promoted owner connects directly to the link server (recipient commands are not proxied) for its user's actions and, as first chain entry, for the count; acceptance names the host and the IP exposure and needs `ADYes` when the settings would route privately; direct-prohibited settings refuse the writes. Link commands use a session of their own; link reads stay proxied. The link server learns when owners act. `linkRcvId` and `creatorQueueKey` authorise nothing. The member-page cursor is a memberId. A pending async swap keeps the owner's keys in the agent's command table, in the same encrypted database as its queue keys.

## 6. Slices

Each slice compiles and passes its tests on its own. simplexmq: `cabal test simplexmq-test --test-options='-m "<pattern>"'` (Postgres with `-fserver_postgres`, on the host); chat: `cabal test simplex-chat-test --test-options='-m "channels"'`, with `use-local-simplexmq` until simplexmq is pinned.

| # | Slice | Est. | Done when |
|---|---|---|---|
| S1 | simplexmq store: `LinkDataHash`, `CONFLICT`, `swapQueueLinkData` (STM, Postgres, journal), `CreateLinkKeys` + `UpdateKeys` logging and replay | ~170 | store, store-log and Postgres store tests |
| S2 | simplexmq protocol: v24, `LSWP`, handler, `LSET` refusal on multi-key queues, client, protocol doc | ~110 | server tests |
| S3 | simplexmq agent: §4.2, sync and async | ~200 | agent tests |
| S4 | chat structural: extract `activateGroupRelays`, the channel intro member creation | ~80 | `-m "channels"` green, no behaviour change |
| S5 | chat wire and data (inert): migration, link fields, events, versions, errors, command functions, commands in record syntax (refusing), optional `groupLink` in `CEvtGroupLinkDataUpdated` and `CRGroupRelaysAdded`, config, hook, state; Bot API docs and generated files (later API changes regenerate them) | ~290 | `-m "channels"`, protocol, JSON and "Bot API docs" tests |
| S6 | link core: locks, `writeChannelLink`, link syncs with no pending kinds yet (mailbox, continuations, `LDATA`/`OK`/`ERR` dispatch), `applyChannelLinkData`, `currentOwner`, multi-owner mode, `linkVersion`, `XGrpInfo` gating, durable rows, intro guard, relay reads and tolerant invitation, main-app gating and chat start | ~270 | T12, existing tests |
| S7 | callers: profile check (core; the editors' rule is in S13–S14), relays (activation, publication, removal, owner joins, capabilities), count publisher and threshold | ~190 | T5 (profile, relays), T11, existing tests |
| S8 | promotion: invite, accept with the direct-connection check, `CLCAddOwner` in syncs with the first-promotion store, commit, M becomes owner | ~230 | T1 (without the role change), T2, T13, T17, T20 |
| S9 | member catch-up with watchdog | ~120 | T10 |
| S10 | roster: claim, owners outside the roster, stored-blob rules, departed authors on owners, members and relays, sync rules (a)–(c), abandonment, relays serving owners, undelivered-roster triggers | ~240 | T1 role change, T5 roles, T9, T15, T21, T22 |
| S11 | removal and leave: removal, removed device, removal wins, admin-removal window, leave with re-sign and wait, destroy | ~170 | T3 (without the history check), T4, T6–T8, T14, T16, T18, T19 |
| S12 | guards: group-link commands, share signing, connect plan, history | ~60 | existing tests, T3's history check |
| S13 | iOS (§6.1) | ~380 | host build, manual checks |
| S14 | Kotlin, mirroring S13 | ~380 | host build, manual checks |

### 6.1 Apps (iOS and Kotlin, mirrored)

Same file split, names and branch order on both platforms.

| Change | iOS | Kotlin |
|---|---|---|
| API: `apiInviteGroupOwner`, `apiAcceptGroupOwnerInvitation(allowDirect)`, `ownerInvitationSent`; `groupLink` optional in `groupRelaysAdded`, `groupLinkDataUpdated`, `AddGroupRelaysResult.added` | `SimpleXAPI.swift`, `AppAPITypes.swift:1001, 1210` | `SimpleXAPI.kt:6815, 6846` |
| Errors: `channelLink(ChannelLinkError)`, one alert per case; SMP `CONFLICT` | `APITypes.swift:744+` | `SimpleXAPI.kt:7807+` |
| `RcvGroupEvent.ownerInvitation(invStatus)`: text, notification, bold like `newMemberPendingReview`; tapping a pending one asks "Become an owner?" (the profile goes to all subscribers; the device connects directly to <host>, revealing its IP address, for channel changes and the count) → accept; on `directConnection`, confirm and repeat with `allowDirect`, or, when `prohibited`, explain the network setting; an accepted one has no action | `ChatTypes.swift:6054+`, `ChatItemView.swift:153-157` | `ChatModel.kt:5447+`, `ChatItemView.kt:734-748` |
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
- Agent: `LDATA` and the sync read carry the hash both swaps accept; a root-signed and an owner-signed swap are read and verified by `getConnShortLink`; a removed signer gets `AUTH`; the async swap notifies `OK`, reports `CONFLICT` as `ERR`, retries while the server is down and resumes after a restart; `deleteLinkQueueWithCreds`; `getConnLinkQueueCreds`; `linkServerSupportsSwap` false on a v23 server.

**chat**, `describe "multiple owners"` under `describe "channels"` (`tests/ChatTests/Groups.hs:281`), relays as in `createChannel2Relays` (`:9616`):

| # | Test |
|---|---|
| T1 | verify, invite, accept; an earlier subscriber accepts M's signed post; a member that knew M through a relay-asserted key ends with the chain key; M edits the profile and changes a role |
| T2 | refusals: unverified; a relay below v23; a link server at SMP v23; M ignores a chain entry with another key |
| T3 | creator promotes M and leaves; M posts through every relay, edits the profile, adds and removes a relay, promotes a verified subscriber, destroys the channel (relays and subscribers process it); a later joiner sees M as owner, the creator departed, verifies the roster, gets the creator's history unsigned |
| T4 | O1 removes O2: O2's device applies the removal on receipt and erases its keys, its next write fails; relays, members and a new joiner treat O2 as departed |
| T5 | with the hook holding O1 between read and swap: O2's profile edit lands, O1's is refused; O1's edit while O2's `XGrpInfo` has not reached it yet is refused; O1 adds and O2 removes relays, both kept; O1 and O2 change roles of different members, every relay and member ends with both |
| T6 | mutual removal: one owner remains |
| T7 | the last two owners leave: one refused |
| T8 | removal wins: O2 removes subscriber M while O1's promotion of M is held by the hook; M ends departed everywhere and the link revokes M |
| T9 | O1 demotes the only admin and leaves while O2 is offline; O2 returns and accepts O1's roster from a relay; members that receive O1's role events after its leave apply them; a joiner gets the roster; the demotion holds |
| T10 | catch-up: earlier subscribers appear on M with keys (it verifies and promotes one); a restart mid-way restarts and skips held members; a silent relay is replaced after the timeout and no member is skipped |
| T11 | count: after the creator leaves, joins and leaves change the published count within the (test-shortened) interval, from the first chain entry only |
| T12 | `linkVersion`: a lower `XGrpInfo` is not applied by relays and members; a stale read does not revert the profile |
| T13 | a single-owner async write queued before the first promotion lands after it and gets `AUTH`; the link keeps M |
| T14 | chain dependency: creator → M → N; M leaves (its entry stays as N's signer); N leaves; writes keep validating |
| T15 | a relay reads the creator's departure before processing its queued blob and still accepts it; a role event from that connection above the link's roster version is refused |
| T16 | a leave stopped after its swap (hook) completes at the next chat start; a departed author's undelivered version is abandoned after the (test-shortened) TTL and an unanswered request, and the next claim proceeds |
| T17 | background work: an acceptance processed while the link server is down promotes once it is back; a restart between the swap and the next read sends the commit once; a relay acknowledgement publishes the relay; a sync whose mailbox relay was removed is redone at chat start |
| T18 | an admin removes M while O1's promotion of M is held: M ends departed everywhere, relays and M's device applying the commit and the removal in either order; an admin's removal of an owner older than the (test-shortened) window is refused |
| T19 | destroy by a device that left after its swap sends no `x.grp.del`; the creator's destroy is refused while its link read fails |
| T20 | a channel-open read held (hook) across a promotion is applied after the commit: M stays an owner everywhere |
| T21 | O2 adds a relay right after O1's role change, and O1 adds one right after its own: each relay gets the current blob, acknowledges and is published |
| T22 | relays not ready when O1 demotes an admin: O1's chat start, and without a restart a relay connection becoming ready, re-signs at the next version and members fetch it; the same with O1 leaving instead (re-sign, wait, leave) |

Existing tests that must stay green: the whole `-m "channels"` group, `ProtocolTests`, `JSONTests`, "Bot API docs", schema dump. Final: `cabal test simplex-chat-test` and `simplexmq-test` (Postgres on the host).

**Manual app checks** (one layout; nothing here depends on layout): invite and accept on two devices with the direct-connection prompt, Owner option and verification routing, Remove owner, last-owner leave refusal, a promoted owner's link screen, composer, relays and web settings, a profile edit refused after another owner's edit.

## 8. Risks

- **Riskiest assumption:** members' and relays' unchanged roster protocol converges when versions are claimed through the link and departed owners' roster messages are accepted by the gate. T5, T9, T15, T21 and T22 in S10; S1–S2 first prove the swap's atomicity and size.
- Background syncs re-derive every callback from the latest read and rows; a callback condition that is not idempotent would send twice or never. T17 in S8 exercises lost events and restarts early.
- Rollout: link servers (presets included) must run v24, and relays chat v23, before promotion works.

## 9. Open questions

Questions 1–3 are decided: role changes need the link server; consent by verified code plus an invitation; member list pulled from one relay without images.

4. `LSWP` sends only the user data. Recommended: as planned; otherwise owners are capped at about 6.
5. `LSET` is refused on queues with several recipient keys (a server behaviour change at v24). Recommended: as planned; it is what stops stale or downgraded single-owner writes from erasing other owners.
6. An admin's removal of a member wins over a concurrent promotion of it, so for `promotionRaceWindow` (1 day) after a device learns of a promotion it applies an admin's removal of that owner. Recommended: as planned; otherwise devices that applied the promotion first keep the member as an owner and the others keep it removed, until an owner removes it.

## 10. Out of scope

Pre-existing single-owner issues: relay forwarding order (`Store/Delivery.hs:178-224`); messages to a backlogged relay dropped (`Internal.hs:2840-2846`); a queued async link write overwriting a newer one (`Agent.hs:1191-1200`); the async `LSET`'s client data lost by its parser (`Agent/Protocol.hs:2322`, `:2075-2079`); admin promotion using relay-reported keys (`Commands.hs:3053`); receivers letting an admin remove an admin in relay groups (`Subscriber.hs:3783-3786`); block and unblock not ordered.

## 11. Follow-up runs

None: the whole feature is planned here.
