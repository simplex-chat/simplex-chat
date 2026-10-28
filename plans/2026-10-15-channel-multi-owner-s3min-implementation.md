# Channel multi-owner (S3-min) — implementation plan

Branch: `f/channel-multi-owner` · Repos: simplexmq (anchored at `cba13823`; S1 starts from `8bc4d360`, the commit simplex-chat pins, and adapts §4.1 to it, §4.1), simplex-chat · Estimated diff: ~2,330 production Haskell lines (simplexmq ~520, chat ~1,810) plus tests, ~380 lines per app platform, and ~850 lines of CI scripts, scripted checks and test plans; sixteen slices, about 112 hours (§6).

The overview, `2026-10-15-channel-multi-owner-s3min.md`, says what owners can do and why; this file says what to build. Anchors are simplex-chat paths under `src/Simplex/Chat/` and simplexmq paths under `src/Simplex/Messaging/` at `cba13823`, except in §4.1, at `8bc4d360`.

## 1. Goal and acceptance criteria

A channel has several equal owners. Any owner can do what the single owner does today; owners are added with the member's consent, can remove each other and can leave; concurrent owner actions are ordered by the link server, so none is lost, overwritten unseen or reverted. Owners are trusted as the single owner is today; the design protects against malicious relays and a link server that withholds or serves stale data.

| # | Criterion | Tests (§7) |
|---|---|---|
| AC1 | An owner invites a member whose code it verified; it accepts; one link write adds its chain entry and key; relays and subscribers treat it as owner; it posts, edits and changes roles | T1, T20, T21 |
| AC2 | Promotion is refused for an unverified member, a relay below `multiOwnerVersion`, a link server below SMP v24, a full key set | T2 |
| AC3 | The creator promotes M and leaves; M does every owner action; a later joiner sees M as owner and the creator departed, verifies the roster, gets the creator's history unsigned | T3 |
| AC4 | An owner removes another; the removed owner's writes and `DEL` get `AUTH`; its device erases its keys; relays, members and joiners treat it as departed | T4, T15 |
| AC5 | Concurrent writes both land or one is refused; a write from a stale copy re-applies to the current data, with no read; an edit over a changed profile is refused; concurrent role changes reach every relay and member, none reverted; mutual removal and the last two leaving leave one owner; parties that applied a removal racing a promotion take the member as owner at their next read | T5–T8, T14, T18, T23 |
| AC6 | A role change made just before its author leaves reaches every device | T9, T15, T22 |
| AC7 | A new owner pulls the member list from a relay; the count keeps updating after the creator leaves | T10, T11 |
| AC8 | An older profile event or link read never replaces a newer profile | T12 |
| AC9 | Background work (promotion, relay publication, count, roster recovery) completes once after an outage or a restart | T16, T17 |
| AC10 | `LSWP` swaps data and keys atomically on every store; a restart replays both | §7 simplexmq |
| AC11 | Single-owner channels behave as today: `-m "channels"` passes | §7 |

## 2. Non-goals

Protection against current or removed owners (they are trusted as the single owner is today); retiring or rotating the root key; ordering block and unblock; relay forwarding order and queueing to backlogged relays; field-level profile merge; equal-version roster tie-break; decline, withdrawal or cancel of owner invitations; profile images in the member catch-up; older apps beyond failing closed.

## 3. Current state

| Where | Today |
|---|---|
| `Internal.hs:1618-1685`, `:2642-2654`, `Commands.hs:2994-2995` | one owner, the creator: owners `[self]` signed by root, the count republished by async `LSET` per join or leave, roster version = gate + 1; the owner role refused in channels |
| `Store/Groups.hs:2353-2373`, `Subscriber.hs:3783-3786`, `:4019-4020` | relays read owners only at invitation and drop an admin's removal of an owner; forwarded content from departed authors is ignored |
| simplexmq `Server.hs:1574-1579`, `Agent.hs:1337-1356`, `:2207-2262` | `LSET` and `RKEY` separate, last writer wins; reads keep the encrypted bytes local; async commands belong to a connection, retried by the agent |

## 4. Design

The link server orders everything owners share. One new SMP command compares the hash of the stored encrypted user data and swaps the user data and, when owners change, the recipient key set in one atomic step; when the hash differs, it answers with the current user data. Every multi-owner link write starts from the owner's stored copy of the link data, applies only its own change, and swaps; a refused swap re-applies the change to the data in the reply, so a write needs a read only without a usable copy, to confirm a refusal the copy shows, or after `AUTH`. The link data commits the roster version and its author, and an owner claims the next version only from the complete roster of the current one, so each version has one author who had all earlier ones: members and relays keep today's roster protocol. A user's command writes synchronously and returns the result; work an owner does in the background is one link sync, made of agent async commands whose continuations re-derive what to do from the latest link data and the database, so it survives restarts and outages.

### 4.1 simplexmq: server

- **Version.** `linkSwapSMPVersion = VersionSMP 24`; `currentClientSMPRelayVersion` and `currentServerSMPRelayVersion` 23 → 24 (`Transport.hs:228-232`); `proxiedSMPRelayVersion` stays 23. History lines in `Transport.hs:158-180` and `protocol/simplex-messaging.md:89-112`.
- **Types and response** (`Protocol.hs`): `newtype LinkDataHash = LinkDataHash ByteString`, the SHA-256 (32 bytes) of the stored `EncDataBytes` contents (nonce and ciphertext, no wire length prefix). A new response `LCUR :: EncUserDataBytes -> BrokerMsg`, tag `"LCUR"`, the queue's current user data, sent only in reply to an `LSWP` whose hash differs; the client computes its hash. No new error.
- **Command:** `LSWP :: LinkId -> LinkDataHash -> Maybe (NonEmpty RcvPublicAuthKey) -> QueueLinkData -> Command Recipient`, tag `"LSWP"`, `e (LSWP_, ' ', lnkId, h, keys_, d)`, parsed `LSWP <$> _smpP <*> smpP <*> smpP <*> smpP`: `LSET`'s fields (link ID, encrypted fixed and user data) with the hash of the user data it replaces and the new key set. A transmission (16,365 bytes, `Protocol.hs:2359-2360`) carries the 2,048-byte encrypted fixed data (`fixedDataPaddedLength` 2,008 plus nonce and tag, `Crypto/ShortLink.hs`), the 13,826-byte user data, the hash and up to 6 Ed25519 keys (45 bytes each). The key set holds the creator's queue key while the creator is an owner and one member key per current owner (§4.4 step 3): at most 5 owners while the creator is one, 6 after it leaves (§4.6). The owner chain (about 128 bytes an entry) fits beside any profile.
- **Handler** (`Server.hs`, near `:1612`): `LSWP lnkId h ks_ d -> withQueue $ \q qr -> checkMode QMContact qr $ maybe OK LCUR <$$> liftIO (swapQueueLinkData (queueStore ms) q lnkId h ks_ d)`. Not in the proxy allow-list (`:2245-2251`).
- **`LSET` on a queue with more than one recipient key gets `AUTH`** (`Server.hs:1612-1616`), checked inside the store's transaction as the swap checks its hash, not on the handler's snapshot, which a concurrent `LSWP` can change; store-log replay of `CreateLink` and `LKEYS` skips the check. Only multi-owner links have several keys (nothing sends `RKEY` today), so stale or downgraded single-owner writes cannot overwrite multi-owner data.
- **Store method** in `QueueStoreClass` (`QueueStore/Types.hs:21-50`): `swapQueueLinkData :: s -> q -> LinkId -> LinkDataHash -> Maybe (NonEmpty RcvPublicAuthKey) -> QueueLinkData -> IO (Either ErrorType (Maybe EncUserDataBytes))`. No link data, or a link ID or fixed data other than the stored ones (as `LSET`'s check) → `AUTH`; hash differs → `Right (Just ud)`, the stored user data; else store the user data and, when given, the key set, `Right Nothing`. The swap, the cached record and the log lines run under `withQueueRec` (`QueueStore/Postgres.hs:619-621`; at `cba13823`, the queue lock, real in the journal store).
  - STM (`QueueStore/STM.hs`): one `atomically` over `readQueueRec`, then the log. The STM message store's queue lock is a no-op (at `cba13823`, `MsgStore/STM.hs:64`); log order rests on the next swap needing this swap's hash, a network round trip away.
  - Postgres (`QueueStore/Postgres.hs`): one `withDB` transaction: `SELECT link_id, fixed_data, user_data FROM msg_queues WHERE recipient_id = ? AND deleted_at IS NULL FOR UPDATE`, compare in Haskell, `UPDATE msg_queues SET user_data = ? [, recipient_keys = ?] WHERE recipient_id = ?`; then the cached record gets the real bytes and keys. The row lock orders swaps in the Postgres-only store, which has no cache and a no-op queue lock (at `cba13823`, `MsgStore/Postgres.hs:84`, `:100`); elsewhere the cache holds placeholder link bytes (`QueueStore/Postgres.hs:588`).
  - Journal: delegate in `QStore` (at `cba13823`, `MsgStore/Journal.hs:304-361`).
- **Store log** (`Server/StoreLog.hs`): a key-changing swap logs a new record `CreateLinkKeys RecipientId LinkId QueueLinkData (NonEmpty RcvPublicAuthKey)`, tag `"LKEYS"` (the parser is an ordered `A.choice`; the tag must not start with `LINK` or `KEYS`), and then the existing `UpdateKeys`; replay (`StoreLog/ReadWrite.hs:37-72`) runs `addQueueLinkData` then `updateKeys`. A swap without keys logs the existing `CreateLink` with the full `QueueLinkData`. A restart never restores data without its keys; an older server skips `LKEYS` but still restores the key set.
- **Client** (`Client.hs`): `swapSMPQueueLink :: SMPClient -> NetworkRequestMode -> RcvPrivateAuthKey -> RecipientId -> LinkId -> LinkDataHash -> Maybe (NonEmpty RcvPublicAuthKey) -> QueueLinkData -> ExceptT SMPClientError IO (Maybe EncUserDataBytes)`, `Nothing` on `OK`, the current user data on `LCUR`; gated `thVersion (thParams c) >= linkSwapSMPVersion`, else `PCETransportError TEVersion` (as `Client.hs:1080-1084`).
- **At `8bc4d360`**, where S1 starts, simplexmq has no journal message store and no Postgres queue cache, and `withQueueRec` takes no queue lock (`QueueStore/Postgres.hs:619-621`); the server keeps memory queues with memory messages and Postgres queues with Postgres messages. S1 builds the swap for these two stores: the journal delegate and the cached-record refresh drop out; a Postgres swap is ordered by its row lock and an STM swap by its one transaction, and the store log by the next swap needing this one's hash. Each Postgres command reads its own copy of the queue record, so the multi-key `LSET` refusal is one more condition on the conditional `UPDATE` in `addQueueLinkData`, as the sender key already is there: `get_byte(recipient_keys, 0) = 1`, the key list's count byte, which the server's down migration tests the same way (`Postgres/Migrations.hs:113`); an `UPDATE` that waits on a concurrent swap's row lock re-checks it on the new row. Store-log replay runs on the STM store only (`StoreLog/ReadWrite.hs:37`), so the Postgres method needs no replay exception. This section's line references are at `8bc4d360`; those marked `cba13823` point at code `8bc4d360` doesn't have.

### 4.2 simplexmq: agent

Every owner, the creator included, writes with explicit credentials: `LinkOwnerCreds {server :: SMPServer, linkKey :: LinkKey, rcvId :: SMP.RecipientId, rootKey :: C.PublicKeyEd25519, rcvAuthKey :: SMP.RcvPrivateAuthKey, sigKey :: C.PrivateKeyEd25519}`. `rcvAuthKey` signs the SMP command and must be a recipient key of the queue: the creator's link-queue key, or another owner's member key. `sigKey` signs the user data: the root key on a device that holds it, otherwise the member key.

An owner writes from the link data it last saw, `OwnerLinkData {encFixedData :: SMP.EncFixedDataBytes, userData :: UserContactData, signedUserData :: ByteString, hash :: LinkDataHash}` (`Encoding` instance; chat stores it, §4.3): the fixed data's encrypted bytes, which `LSWP` repeats, the parsed user data, the decrypted user data as signed, and its hash. `signedUserData` holds the bytes the parsers pass over (`UserContactData`'s unparsed tail, `Agent/Protocol.hs:2078`, and each owner entry's extension, which `OwnerAuth`'s parser drops, `:1987`); the swaps take the base `OwnerLinkData` and re-emit, from its `signedUserData`, the tail and the raw owner entries the write leaves unchanged, so an older owner's write keeps fields a newer owner's agent wrote. A swap returns the link's data after it, `LinkSwapResult = LSRSwapped OwnerLinkData | LSRChanged OwnerLinkData`: the data it wrote with the hash of the bytes it sent, or, when the hash differed, the server's current data, decrypted and verified against `rootKey` as `getConnShortLink'` verifies a read.

| Function | Does |
|---|---|
| `getConnShortLinkForWrite :: AgentClient -> NetworkRequestMode -> UserId -> ConnShortLink 'CMContact -> AE (FixedLinkData 'CMContact, OwnerLinkData)` | `getConnShortLink'`'s contact branch (`Agent.hs:1337-1356`), proxied as `LGET` is, keeping the encrypted fixed data and the user data's hash |
| `setShortLinkSwap :: AgentClient -> NetworkRequestMode -> UserId -> LinkOwnerCreds -> OwnerLinkData -> Maybe (NonEmpty RcvPublicAuthKey) -> UserContactData -> AE LinkSwapResult` | require `sigKey`'s public key to be `rootKey` or in `owners`, and `validateLinkOwners rootKey owners` (`Agent/Protocol.hs:2003-2017`); `encodeSignUserData` with `sigKey`, keeping the base's passed-over bytes, `encryptUserData` with the key from `contactShortLinkKdf linkKey`; `LSWP` with the link ID from `linkKey`, the base's fixed data and hash, signed with `rcvAuthKey` |
| `setShortLinkSwapAsync :: AgentClient -> ACorrId -> ConnId -> LinkOwnerCreds -> OwnerLinkData -> Maybe (NonEmpty RcvPublicAuthKey) -> UserContactData -> AE ()` | the same as an async command on `(connId, Just server)`, `connId` a mailbox only (`Agent.hs:1206-1210`); event `LSWAP LinkSwapResult` |
| `deleteLinkQueueWithCreds :: AgentClient -> NetworkRequestMode -> UserId -> LinkOwnerCreds -> AE ()` | `DEL` with `rcvAuthKey` |
| `getConnLinkQueueCreds :: AgentClient -> ConnId -> AE (SMP.RecipientId, SMP.RcvPrivateAuthKey)` | the creator's link queue ID and key, read locally as `getConnLinkPrivKey` reads the signing key (`Agent.hs:494-496`) |
| `linkServerSupportsSwap :: AgentClient -> NetworkRequestMode -> UserId -> SMPServer -> SMP.LinkId -> AE Bool` | connect on the link session, compare the negotiated version with v24 |
| `linkServerPrivateRouted :: AgentClient -> UserId -> SMPServer -> AM Bool` | the settings route to this server privately and the connection is not IP-protected (`shouldUseProxy`, `ipAddressProtected`, `Agent/Client.hs:1190-1211`) |

- **Async read for writing.** `LDATA` gains the encrypted fixed data and the hash (`Agent/Protocol.hs:407`); `getConnShortLink'` keeps them from the bytes it read (`:1339`), the sync `getConnShortLink` drops them, and the chat match (`Subscriber.hs:1245`) and agent tests take the fields. `getConnShortLinkAsync` is unchanged.
- **Async swap command.** `ACommand` gains `LSWP LinkOwnerCreds OwnerLinkData (Maybe (NonEmpty RcvPublicAuthKey)) UserContactData` (`Agent/Protocol.hs:466-475`), tag `"LSWP"`, `strEncode` with the keys as `C.StoredPrivateKey`, the base before the new user data, and the new user data last, since its parser consumes the rest (`:2075-2079`); `AEvent` gains `LSWAP LinkSwapResult`; worker branch `withServer' . tryCommand $ setShortLinkSwap' c NRMBackground userId creds base ks_ ud >>= notify . LSWAP` (as `LSET`, `Agent.hs:2255-2258`), reporting `TEVersion` as `ERR` instead of retrying it as a host error (`Agent/Client.hs:1627-1637`). A retry after a lost reply re-encrypts with a new nonce and sends the same hash; if the first attempt landed, the reply is `LSRChanged` holding the data it wrote. Commands are blobs without a version (`Agent/Store.hs:509-520`): no migration.
- **Link session.** `getConnShortLinkForWrite` when not proxied, the swaps, `DEL` and the version check use one transport session per link, `(userId, server, Just linkId)` (`withClient`/`withLogClient`, `Agent/Client.hs:1153-1159`), not the one shared per user and server (`mkTSession`, `:1442-1444`), which would tie them to the user's other queues there. Other reads are unchanged.
- **Direct connections.** The creds calls and `linkServerSupportsSwap` fail with `CMD PROHIBITED` when `linkServerPrivateRouted` holds and the agent's policy allows no direct fallback (`directAllowed`, `Agent/Client.hs:1204-1208`). Owner validation fails with `AGENT A_LINK`.
- `Crypto/ShortLink.hs`: extract and export `mkOwnerAuth :: OwnerId -> C.PublicKeyEd25519 -> C.PrivateKeyEd25519 -> OwnerAuth`; `newOwnerAuth` (`:81-85`) uses it.
- Errors chat sees: `SMP _ AUTH` (key not in the set, or a link ID or fixed data not the stored ones); `BROKER _ (TRANSPORT TEVersion)`; `CMD LARGE` (user data over 13,782 bytes); `CMD PROHIBITED`. The async worker retries temporary and host errors without limit and reports the others as `ERR` (`Agent.hs:2446-2466`).

### 4.3 Link data, owners, modes

`GroupShortLinkData` (`Protocol.hs:1621-1631`) gains optional fields; `UserContactData` is unchanged and a write carries its `direct` and `ratchetKeys` from the data it starts from.

| Field | Type | Written | Meaning |
|---|---|---|---|
| `linkVersion` | `Maybe Int64` | every swap: `max(read, link_data_version) + 1`; single-owner writes carry none | write counter; absent counts as 0 |
| `revokedOwners` | `Maybe [MemberId]` | removal and leave | departed owners; never shrinks |
| `creatorQueueKey` | `Maybe CreatorQueueKey {memberId :: MemberId, key :: SMP.RcvPublicAuthKey}` | the creator, in every swap from its first one | its link-queue recipient key |
| `linkRcvId` | `Maybe SMP.RecipientId` | the creator, in every swap from its first one | queue address for other owners; authorises nothing |
| `roster` | `Maybe LinkRoster {version :: VersionRoster, author :: MemberId}` | every roster claim and the creator's first swap | the committed roster version and its author |

Keys, IDs and `OwnerAuth` encode in JSON as base64url of their `smpEncode`. A write merges the encoded record over the JSON object of the data it starts from (left-biased union), keeping keys this version does not know, which `GroupShortLinkData` drops on parse today.

**Current owner** (`currentOwner m = memberRole' m == GROwner && not (memberRemoved m)`) is used for every owner check below; owner rows from the chain or a commit have status `GSMemUnknown` (`createLinkOwnerMember`, `Store/Groups.hs:3575-3603`), which `memberCurrent` excludes.

**Multi-owner mode** (`isMultiOwnerChannel`), on a device whose membership is a current owner: it holds an owner row other than its own membership (any status), a member's `owner_inv_accepted = 1`, or `link_data_version` is set (the link has had a swap). From the first accepted invitation every link write of the device goes through §4.4; single-owner channels, and a creator whose invitations nobody accepted, keep `setGroupLinkData`/`setGroupLinkDataAsync` (`Internal.hs:1588-1604`) unchanged. The creator's first swap makes the queue multi-key (§4.4 step 3), so from then on its queued single-owner writes get `AUTH`.

**Credentials** for a write are built each time: the channel link's server and key, `rcvId` and `rcvAuthKey` from `getConnLinkQueueCreds` on the creator and from the data's `linkRcvId` and the member key elsewhere, the group's root public key, and `sigKey` (§4.2).

**Stored copy.** An owner device in multi-owner mode keeps the last link data it saw in `groups.link_owner_data`: the agent's `OwnerLinkData` (§4.2), the user data as signed, so fields this version does not know are kept. Every read, swap reply and channel open on that device replaces it, under the group lock, unless its `linkVersion` is below the copy's (`applyChannelLinkData` step 1). Writes are built from it (§4.4): the swap's hash check makes a write land only if the copy is current, and otherwise brings back the current data to build again. A write ends without a swap only on data confirmed in that attempt, a swap's reply or a read: a refusal or "no write" found on the stored copy (last owner, already applied, changed by another owner, owner limit, roster behind) is confirmed by one read, which replaces the copy, and decided again on it. A write reads first when there is no usable copy (a new owner's first write, the creator's first swap, a copy whose `linkVersion` is below `link_data_version`). The device also reads, routed as the user's private routing settings route every link read, at channel open, before destroying, after `AUTH`, and in `LSResign` syncs (chat start, the iOS app's return to the foreground, relay set-up and readiness, an undelivered roster). It costs one column, in the change's one migration, of up to about 16 KB (the fixed and user data at their padded sizes and the hash) per multi-owner channel on owner devices, and reads and writes of it under the group lock.

### 4.4 Writes, reads, link syncs

**Locks.** `ChatController.linkWriters :: TMap GroupId Lock` serialises this device's user-command link writes per channel. Network calls never run under the group lock, which every agent event of the channel takes on the single event thread (`Subscriber.hs:146-147`); DB steps take it briefly.

**`writeChannelLink :: User -> GroupId -> ChannelLinkChange -> (LinkWrite -> ChannelLink -> CM a) -> CM a`** (`LinkWrite = LWWritten | LWUnchanged`), for user commands, under the link writer lock:

Below, "the read" is the link data a step works from: the stored copy (§4.3), a read, or a swap's reply.

1. Start from the stored copy; without a usable one (§4.3), read with `getConnShortLinkForWrite` on `publicGroup.groupLink`. Decode `UserContactData` and `GroupShortLinkData` with its raw JSON; `channelLinkWriteHook` (tests).
2. `continueChannelLink` (below) with this change; `Right Nothing` = already applied, no write. A refusal or `Right Nothing` found on the stored copy, not on a read or a reply of this attempt, is confirmed first: read once with `getConnShortLinkForWrite` and step 2 again. If the read shows this device departed: stop with `CLENotOwner` (a leave instead completes its departure, §4.9).
3. When the owner set changes, or the read lacks `creatorQueueKey`, `linkQueueKeys`: `creatorQueueKey.key` unless the creator is revoked, plus the `ownerKey` of every chain entry not in `revokedOwners`, the creator's own included (two at least while the creator is an owner). A creator's write whose read lacks `creatorQueueKey` also writes it and `linkRcvId`, and, if the read has no roster, `roster_version` is set and the change is not a roster claim, stores its rebuilt roster as its own (§4.10) and commits `roster {roster_version, self}`.
4. `linkVersion` = `max(read, link_data_version) + 1`; `setShortLinkSwap` with the read as the base.
5. `LSRSwapped d` → `LWWritten`, with `d` applied (step 6); for `CLCRoster`, an `LSRChanged d` whose roster is the version this attempt claimed, by this owner, is `LWWritten` too (its reply was lost). `LSRChanged d` → step 2 with `d`, at most `channelLinkWriteAttempts` (5) swaps, then `CLELinkBusy`. A network error or timeout → the same swap once more: if the first landed, the reply is `LSRChanged` with this write's data, which step 2 finds already applied. `AUTH` → one read and apply (it shows the revocation), `CLENotOwner`. `TEVersion` → `CLELinkServerVersion`. `CMD LARGE` → `CLELinkDataTooLarge`. `CMD PROHIBITED` → `CLEDirectConnection`.
6. On success (`LWWritten`), and on "already applied" (`LWUnchanged`), under the group lock, while the membership is current: store the written data as the copy, `max` of the stored and the link's `linkVersion` in `link_data_version` and its roster commitment in `link_roster_version` (as `applyChannelLinkData` step 5), and run the callback: local state, events to send, the result.
7. Release the writer lock; enqueue an `LSSync` if the read left background changes pending (below), or an `LSResign`, which reads, if the write failed after an attempt that may have landed.

**`continueChannelLink :: User -> GroupInfo -> ChannelLink -> [ChannelLinkChange] -> CM (Either ChannelLinkError (Maybe ChannelLink))`**, the step both paths share, under the group lock: `applyChannelLinkData` (below, which also keeps the data as the copy); run the callbacks the read shows due (the table below, and rule (a) of §4.10 while no user write holds the link writer lock); then `applyLinkChange :: ChannelLinkChange -> ChannelLink -> CM (Either ChannelLinkError (Maybe ChannelLink))` for each given change in turn. It returns the link to write, or `Nothing` when no change is left to write.

Callers never hold the group lock across this call. In multi-owner mode only, commands that hold `withGroupLock` across their body today take it only around their DB steps: `APIMembersRole` (`Commands.hs:2984`), `APIRemoveMembers` (`:3123`), `APILeaveGroup` (`:3236`), `APIDeleteChat` (`:1390`), `APIAddGroupRelays` (`:2781`); their other paths keep it.

**Link sync**, the background path: `enqueueLinkSync :: User -> GroupInfo -> LinkSyncKind -> CM ()` (`LSSync | LSResign`, §4.10), main app only (§4.15). The count timer's sync is skipped while any channel-link command row of the group (sync, resign or swap), under 10 minutes old, is pending, since its continuation re-derives the count; other triggers always enqueue. `enqueueLinkSync` is called with the group lock held: event handlers hold it, and the count timer, step 7 and channel open take it first. The mailbox is the owner's `ConnReady` connection to a current relay (an `ERR` does not fail it, `Subscriber.hs:179`; its events take the group lock), preferably one whose receive queue is not on the link server, as the agent runs one worker per connection and server (`Agent.hs:2226-2228`), then the lowest member ID; without one there is no sync until the next trigger. Deleting the mailbox connection, as departure does, deletes its pending commands and their keys once the agent completes it (a swap that runs first gets `AUTH`). New `CommandFunction`s (`Types.hs:2162-2225`): `CFChannelLinkSync`, `CFChannelLinkResign` (expect `LDATA_`), `CFChannelLinkSwap` (expects `LSWAP_`).

1. An `LSSync` with a usable stored copy runs step 2 on it at once, under the group lock its caller holds; a refusal found on the copy is confirmed as in the user path's step 2, by a read (`getConnShortLinkAsync`). An `LSResign` sync, or one without a copy: `createCommand db user (Just mailbox) fn`, then `getConnShortLinkAsync` with the mailbox (`Internal.hs:3178`); its read finds work a lost event or a crash left.
2. Continuation, on the copy, on `LDATA` (`processGroupMessage`, `Subscriber.hs:1245`) and on a swap's `LSWAP`, under the group lock: decode; `channelLinkWriteHook`; `continueChannelLink` with the pending changes (a refusal is handled per change); if this device is departed, stop. If it returns a link to write and `ChatController.linkSyncSwaps :: TMap GroupId Int`, this channel's swaps since its last trigger, is below `channelLinkWriteAttempts`: key set, creator fields and `linkVersion` as in steps 3–4, `createCommand … CFChannelLinkSwap`, `setShortLinkSwapAsync` with the read as the base.
3. Swap events (`Subscriber.hs:1234-1236`, `:1316-1320`, dispatched on the function before any view event): `LSWAP` → step 2 with its data, so no read follows a swap: a landed swap's data shows its changes applied and step 2 runs their callbacks; a refused one's is the current data, to which step 2 re-applies what is still pending; a landed swap the agent retried after a network error comes back refused, holding its own data. `AUTH` → an `LSResign` sync, whose read shows the revocation, with no view event. Other `ERR`: a view event, stop. Only a trigger resets the swap count, so a link server refusing swaps, or accepting them and serving old data, cannot keep a device looping.

Only the command row is carried between steps; every decision comes from the latest link data and the database, so a lost or repeated agent event (`Agent.hs:2459`, `:2467-2470`) or a crash between steps is absorbed by the next trigger or chat start.

**Changes**, with when a sync finds each pending and the callback it runs once the link data shows it applied:

| `ChannelLinkChange` | Applies | No write / refuses | In a sync: pending when → callback |
|---|---|---|---|
| `CLCProfile base new` | the profile, whole | read = `new` → no write (landed); read ≠ `base` → `CLEChangedByOwner` | user commands only |
| `CLCAddOwner member ownerAuth` | entry after the signer's; key into the set | member revoked or departed → refuse; already in the chain → no write | a member has `owner_inv_accepted = 1` → §4.6 step 3 |
| `CLCRevokeOwner memberId` | memberId into `revokedOwners`; its key out | already revoked → no write; no other current owner would remain → `CLELastOwner` | user commands only |
| `CLCRelays added removed` | read relays + added − removed − relays held as left | no change → no write | an acknowledged relay (`RSAcknowledgedRoster`) missing, or a left one listed → `activateGroupRelays` (§4.13) |
| `CLCCount n` | `publicGroupData.publicMemberCount = n` | not the first current chain entry, or read count = `n` → no write | `members_catchup = 0`, tally ≠ read count → none |
| `CLCRoster claim` | `roster` (§4.10) | §4.10 | §4.10 rules (b), (c) → rule (a) |

Triggers: acceptance (§4.6); a relay's acknowledgement or own leave (§4.13); the count timer (§4.11); a relay connection ready or resuming, relay set-up (§4.10, `LSResign`); a user write or channel open whose read shows pending work; chat start (§4.15).

Chain rules: signing order is kept (`validateLinkOwners`); the creator signs new entries with the root key (`Commands.hs:2757`), others with their member key (`mkOwnerAuth`); a revoked entry is dropped once no current entry depends on it, transitively, except the writer's own entry in its leave write (a non-root writer must be in the chain, `Agent/Protocol.hs:1996-2001`).

**`applyChannelLinkData`**, under the group lock, on all channel link data (reads, and owners' copies and swap replies): members at channel open (`APIGetUpdatedGroupLinkData`, `Commands.hs:1959-1972`) and connect plan (`:4624`, `:4651`); owners at channel open, in syncs and in writes; relays at invitation (`getLinkDataCreateRelayLink`, `Subscriber.hs:4585-4606`) and in relay checks (`checkRelayServedGroups`, `Commands.hs:6001-6020`); joiners in `APIConnectPreparedGroup` (`Commands.hs:2303-2333`).

1. A read whose `linkVersion` (absent = 0) is below `link_data_version` is ignored; else store it, and an owner device in multi-owner mode keeps the read as its copy (§4.3) unless it is older than the copy. A swap's own reply below `link_data_version` (another owner's `XGrpInfo` came first) is neither applied (steps 2–6) nor kept as the copy, but `continueChannelLink` still runs the callbacks it shows due and applies the given changes to it.
2. Profile and count as `updateGroupFromLinkData` (`Internal.hs:1633-1654`).
3. `revokedOwners`: each held member becomes departed (`GSMemLeft`) at whatever role it is held; unknown memberIds get departed rows. A relay keeps a departed owner's connection (§4.13). Own memberId listed while the membership is current: complete the departure (§4.9 step 3).
4. Non-revoked chain entries: a member not held as owner becomes owner with `ownerKey` (created with a placeholder profile if unknown), a departed one included (`GSMemUnknown`, §4.3), so every party follows the link (§4.8); its own entry with its own key and a current membership: become owner (§4.6); with another key: alert item, stay a subscriber. A read only adds owners and marks revoked ones departed, so one older than a promotion cannot undo it.
5. `roster` → `link_roster_version`; `link_roster_seen_at` = now when the version changes.
6. Owners: join each listed relay without a relay member (`connectToRelayAsync`, `Internal.hs:1606-1616`); `RSInactive` for active `group_relays` rows missing from the list (as `Subscriber.hs:1466-1477`). When the read `roster.version` is above `stored_roster_version`, `XGrpRosterRequest` to every current relay (§4.10).

Owners read on channel open in multi-owner mode, with `getConnShortLinkForWrite` (the same proxied `LGET`), so the read replaces the copy; for a single-owner creator `APIGetUpdatedGroupLinkData` returns the group without a read, keeping today's behaviour.

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

Each event: constructor, tag, parser and JSON (`Protocol.hs:460-1557`), `requiresSignature` (`:1357-1371`), `isForwardedGroupMsg` (`:528-549`), relay and forwarded dispatch (`Subscriber.hs:1080-1132`, `:4041-4064`), Bot API docs (`bots/src/API/Docs/*.hs`, generated files checked by `tests/APIDocs.hs`). Older clients ignore them.

### 4.6 Promotion with consent

**Invite.** `APIInviteGroupOwner {groupId, groupMemberId}` (CLI `/_owner invite #<groupId> <groupMemberId>`, `/owner invite #<group> <member>`) → `CROwnerInvitationSent {user, groupInfo, member}`. Refused unless:

- the user is a current owner of a channel; M is a current member with a key, not an owner, not departed;
- M's code is verified: `memberVerifiedCode` equals `getChannelMemberCode` now (`Commands.hs:3811-3815`), else `CLENotVerified` (only this out-of-band check detects a relay that substituted M's key, as it could sign the acceptance too);
- every current relay supports `multiOwnerVersion`, else `CLERelayVersion`; the key set (§4.4 step 3) with M's key counts at most 6 keys (§4.1; the creator's queue key counts while the creator is an owner), else `CLEOwnerLimit`, counted on the copy and, when it refuses, on a read (§4.3);
- `linkServerSupportsSwap`, else `CLELinkServerVersion` (`CMD PROHIBITED` → `CLEDirectConnection {host, prohibited = True}`).

It sends `x.grp.owner.inv {memberId}` and stores the message's shared msg id in M's `owner_inv_msg_id` (a new invitation replaces it). M's version is not checked (known only from M's join, `Store/Groups.hs:2533-2551`); an older M ignores the invitation.

**Receivers.** Relay: sender a current owner, M current → forward to M's support scope. M: author a current owner and the memberId its own → item `RGEOwnerInvitation {invStatus = OISPending}` in its support scope (`mkGetMessageChatScope`, `Internal.hs:1984-2003`, which sets `supportChat`). Others: no item.

**Accept.** `APIAcceptGroupOwnerInvitation {groupId, chatItemId, allowDirect :: AllowDirect}` (`ADAsk | ADYes`; CLI `/_owner accept #<groupId> <chatItemId>[ direct=on]`) on M: a pending invitation, the membership current, not owner. When the creds calls would get `CMD PROHIBITED` (§4.2): `CLEDirectConnection {host, prohibited = True}`. `ADAsk` and `linkServerPrivateRouted`: `CLEDirectConnection {host, prohibited = False}`; the app asks and repeats with `ADYes`. It sends `x.grp.owner.acpt {invId}` (the item's shared msg id), sets the item to `OISAccepted`, returns `CRChatItemUpdated`. Relays forward it to M's support scope; the promoter acts when `invId` equals M's `owner_inv_msg_id`.

**Promote.** On the acceptance the promoter sets M's `owner_inv_accepted = 1` and enqueues a link sync. In the sync (§4.4):

1. Re-check M (current, not departed or revoked), the relays' versions and the key set's limit against the read; a failure that persists (`CMD LARGE` included) clears the invitation and is shown. The creator's first swap also writes its own fields and roster commitment (§4.4 step 3; the stored roster is rebuilt without M, `buildGroupRoster`).
2. `CLCAddOwner M (mkOwnerAuth …)`.
3. Callback, when the link data (the swap's reply, or any later read) shows M's entry with M's key while M's `owner_inv_accepted = 1` (later data finds it cleared and sends nothing): clear `owner_inv_msg_id` and `owner_inv_accepted`; M's role owner (its key is the chain key); send `x.grp.owner.add {memberId, memberKey, profile}` with M's redacted profile (`redactedMemberProfile`, `Internal.hs:1361`); item `SGEMemberRole … GROwner`; `CEvtMemberRole`.

**Commit.** Relays, members and owners apply `x.grp.owner.add` when the sender is a current owner and `memberId` is not held as departed: role owner, `member_pub_key = memberKey` (warning if it replaces a different key), the member created from `profile` if unknown, the role item as `xGrpMemRole` creates it; relays send their capabilities (§4.13). A party holding `memberId` as departed takes it as owner at its next read (§4.8). It is not `x.grp.mem.role … owner`, so older clients ignore it and no relay-asserted key is promoted (`Subscriber.hs:3465-3468`).

**M becomes owner** when it applies the commit about itself or reads its own entry (§4.4), with its own key and a current membership: role owner, item `RGEUserRole GROwner`, `members_catchup = 1`, member catch-up (§4.7). It stores no credential: each write builds them.

### 4.7 New owner's member list

- **Request.** `x.grp.members.req {cursor}` to one current relay with a ready connection; `cursor = Nothing` starts from the beginning. A watchdog (`ChatController.memberCatchups :: TMap GroupId (Int, Maybe MemberId)`, a request counter and the cursor) re-requests from the same cursor at the next ready relay when no page arrived within `memberCatchupTimeout` (1 minute); it is a forked delay, the request an ordinary message. Main-app chat start restarts from the beginning while `members_catchup = 1`.
- **Relay.** Serves a current owner only (silence otherwise). One page: current members (statuses of `getGroupMembersByCursor`, `Store/Delivery.hs:352-390`) with `member_id > cursor ORDER BY member_id` (the same on every relay, indexed by `UNIQUE(group_id, member_id)`; the cursor's own row may be gone), excluding relays and the requester; `memberInfo` with the redacted profile and no image, plus restrictions, as many as fit one message (`maxEncodedMsgLength`, `Protocol.hs:929`; 40–60); `cursor` = the last packed memberId, absent at the end.
- **Owner.** A page from a current relay while `members_catchup = 1`, in one transaction: a held `GSMemUnknown` member gets its profile and status as `xGrpMemNew` gives them (`updateRosterMemberAnnounced`, `Subscriber.hs:3243-3251`), never role or key; departed and other held members are skipped; new ones are created by `xGrpMemIntro`'s channel rule (`Subscriber.hs:3326-3334`, extracted): roles from member up at the default role without a key (the roster establishes those), subscribers with the relay-reported key. No per-member link write. Then the next request, or `members_catchup = 0`. A page from a relay asked earlier is applied; only the relay asked last gets the next request.

### 4.8 Removing an owner

- **Command.** `APIRemoveMembers` with a current owner as the only target, by a current owner, in a channel: `writeChannelLink` with `CLCRevokeOwner X`; callback: mark X removed (rows kept, §4.14), send `x.grp.mem.del X False Nothing` (`deleteMemsSend`, `Commands.hs:3191-3229`). Removing oneself is a leave. After a crash between swap and callback, parties learn from link reads.
- **Receivers** apply it as today (`checkRole`, `Subscriber.hs:3783-3786`); a relay forwards it to X first, then deletes X's connection (`:3755-3761`).
- **The removed owner's device** applies it on receipt (own-membership branch, `Subscriber.hs:3734-3746`), as it does a read showing itself revoked (§4.4 step 3): membership removed, keys erased (§4.9 step 3), catch-up stopped.
- **A removal racing a promotion** reaches parties in either order, and the promotion wins, with nothing done for it. A party that applied the removal first refuses the commit and takes M as owner at its next read (§4.4 step 4); one that applied the commit refuses an admin's removal of an owner as today (`checkRole`, `Subscriber.hs:3783-3786`). If M's own device applied the removal first, it has left (`Subscriber.hs:3736-3741`), and relays that applied it first have deleted their connection to M: M stays listed as an owner until an owner removes it. A member that leaves while being promoted ends the same way.
- Mutual removal: the first swap removes the other's key; the second gets `AUTH`, or a refused swap whose data shows itself revoked.

### 4.9 Leaving

A single-owner creator first reads the link, before the group lock: a read with `creatorQueueKey` (a restore from an older backup) is applied, making it multi-owner; otherwise it keeps `Commands.hs:3233-3277`, and a failed read refuses the leave (that path's retried `DEL` would delete a multi-owner link). `APILeaveGroup` by an owner in multi-owner mode:

1. From the link data (§4.4 steps 1–2: a refusal found on the copy is confirmed by a read): refuse with `CLELastOwner` if no other current owner. If `roster.author` is this owner and either `stored_roster_version` is below `roster.version` (its claim landed but was not stored) or its stored roster is undelivered (`roster_pending_msg_id` set, §4.10), make an `RCResign` claim first (§4.10, through `writeChannelLink`; it sends the blob and one role event) and wait up to `channelOwnerWaitTimeout` (30 s), re-reading the column without the group lock (`SENT` is processed under it), until a relay has `SENT` the new blob's last chunk; else refuse with `CLERelaysUnreachable`.
2. `writeChannelLink` with `CLCRevokeOwner self` (own entry kept, key dropped); the last-owner check runs on every attempt.
3. Complete the departure, only while the membership is current: `x.grp.leave` to relays; delete member connections waiting for delivery; membership `GSMemLeft`; erase `member_priv_key` (a later load generates an unrelated key, `Store/Shared.hs:750-762`); the creator replaces `root_priv_key` with the public key and deletes its group link (`deleteGroupLinkIfExists`: `DEL` gets `AUTH` and the agent deletes the queue locally, `Agent.hs:3080-3088`).

The next link data (chat start's read, a channel open, a swap's reply) completes a leave that crashed after step 2 (§4.4 step 3); a repeated leave whose data shows itself revoked completes step 3 and returns `CRLeftMemberUser`.

### 4.10 Roster ordered by the link

**Claim.** In multi-owner mode `APIMembersRole` and `APIRemoveMembers` call `claimRosterVersion` instead of `broadcastRoster` (`Commands.hs:3005-3008`, `:3136-3139`): `writeChannelLink` with `CLCRoster (RCChange delta targets startRoles)`, where `startRoles` are the targets' roles loaded when the command started. `RosterClaim = RCChange RosterDelta [GroupMember] [GroupMemberRole] | RCResign`. With `r` = read `roster.version` and `s` = `stored_roster_version`, the version of the complete roster this owner holds (its own claim or a blob it applied; none = −1). The complete frontier is not the test: it advances on the first delta of a version (`Subscriber.hs:3417-3421`), and a multi-member change has several. A read without `roster` (before the creator's first swap lands, or in a channel that never had one) gives `r` = `s` = this device's `roster_version`, so a claim never goes below members' gate:

1. If `s < r`: if the read author is this owner (its claim `r` was lost before it was stored or sent), claim from its own state; if the author is departed and the abandonment rule holds (below), claim `r + 2` from its own state; otherwise send `XGrpRosterRequest` with `s` to every current relay and refuse with `CLERosterSyncing`.
2. `RCChange`: any target role ≠ its start role → `CLEChangedByOwner`.
3. Build the blob at the claimed version `v` (`r + 1`, or `r + 2` when abandoning) from current roster members with the delta (`buildGroupRoster (applyRosterDelta …)`, `Internal.hs:2629-2654`); `roster = {v, self}`.
4. Callback, only on `LWWritten` (an unchanged link claimed nothing; return `Nothing`): `setOwnGroupRoster` sets `roster_version`, `applied_complete_roster_version`, `stored_roster_version` to `v`, `roster_blob`, sender = own membership (header columns NULL); `sendStoredRoster`; return `v` for the events.

An owner that applies another owner's versioned event above `s` requests the roster from every current relay, as a read does, while `rosterRequests` holds no request (below), so a later claim usually finds it complete; owners in multi-owner mode skip the frontier gap request (`requestRosterOnGap`, `Subscriber.hs:3436-3446`).

Single-owner channels keep `broadcastRoster`. Owners outside the roster: `processRosterEntries` (`Subscriber.hs:3610-3630`) skips current owners, the user's own membership included, and departed members; `xGrpMemRole` ignores owner targets and the owner role; `APIMembersRole` refuses owner targets and the owner role in channels.

**Stored blob.** `getOwnStoredRoster` returns `(stored_roster_version, roster_blob)`; `sendStoredRoster` sends `XGrpRoster {version, sha512 of the bytes}` and the chunks of those exact bytes (`sendRoster`'s steps without re-encoding, `Internal.hs:2665-2681`), and records the version in each relay member's `roster_served_version` (unused on owners today). In multi-owner mode a relay gets a roster only this way and only when `s` equals `link_roster_version`, the commitment last read or written; otherwise the owner requests the roster with `s` (relays serve when theirs is greater) and sends after completion. This covers relay set-up (`Subscriber.hs:937-942`: in multi-owner mode it creates no version; with no roster in the link it enqueues an `LSResign` sync), the acknowledgement (`xGrpRosterAck`, `:3654-3672`: compared with that relay's `roster_served_version`; a lower one re-sends once per newer stored version) and every re-send.

**Departed authors.** Owners and members process a forwarded `XGrpRoster` header and versioned `XGrpMemRole` and `XGrpMemDel` from a departed owner by the roster gate, as from a current one (`xGrpMsgForward`, `Subscriber.hs:4019-4020`; roster chunks do not pass there, `:1131`); other content stays ignored. A served header reuses its shared msg id (`Internal.hs:1293-1307`) and is dropped as a duplicate (`:2956-2959`), so on an owner a duplicate `XGrpRoster` header still goes to `xGrpRoster` when no transfer from that relay is pending and `s` is below its version, letting another relay complete a stalled fetch. A relay processes them on a departed owner's kept connection up to `link_roster_version` (§4.13). Relays answer a current owner's `XGrpRosterRequest` whenever stored > requested (`xGrpRosterRequest`, `:3679-3685`), without the served-version bound kept for members.

**Link sync rules**, the background roster work:

- (a) In every sync, when the read author is this owner with `stored_roster_version` below the read version, and no user write of this device holds the link writer lock (tested without waiting; a held lock means a claim between its swap and its callback): store its own roster at that version (`setOwnGroupRoster`), `sendStoredRoster`, and one `x.grp.mem.role` at that version: the current role of the first current non-owner, non-relay member with a key, by memberId. Any event at that version makes members that missed earlier ones see a gap and fetch the blob (`applyAtRosterVersion` acts before the target lookup, `Subscriber.hs:3410-3424`).
- (b) In every sync, when `s < r`, the author is departed and the abandonment rule holds: an `RCResign` claim of `r + 2` from its own state.
- (c) In `LSResign` syncs only, when the read author is this owner at its stored version and `roster_pending_msg_id` is set, or the read has no roster (relay set-up needs a committed one): an `RCResign` claim.

An `RCResign` claim writes `roster {v, self}` with no delta; in a sync rule (a) then sends it, and through `writeChannelLink` its callback does the same. A sync never sends an event at a version it is re-sending.

**Abandonment.** A version `r` by a departed author, with `s < r`, is abandoned (claim `r + 2` from its own state: no relay serves `r`'s blob, and the skipped version makes devices that applied some of `r`'s events see a gap and fetch the new roster) when `link_roster_seen_at` is older than `rosterAbandonTTL` (1 day) and a request to every current relay at least `rosterRequestTimeout` (5 minutes) earlier brought no blob at `r`: `ChatController.rosterRequests :: TMap GroupId UTCTime` keeps the time of the first unanswered request (set when a read, claim or early request sends one to every current relay while behind, cleared when an applied blob brings `s` to the read version). Claims and syncs check it when they need it; no timer. An owner that also missed that author's earlier versions (`s < r − 1`) drops them too, though some devices received them; otherwise roles could never change again. A current author's version is never abandoned: its device sends it when back (rules (a), (c)); a lost device is removed first.

**Undelivered roster.** `setOwnGroupRoster` sets `groups.roster_pending_msg_id` to 0 ("not sent") in its transaction; `sendStoredRoster` of a roster this owner authored sets it to the last chunk's message; the relay `SENT` handler (`Subscriber.hs:1189-1194`) clears it for that message, and applying another owner's blob clears it. A message to a relay that is not ready is dropped (`memberSendAction`, `Internal.hs:2840-2846`), leaving it set. At main-app chat start, on a relay's `CON` (`:889`) and when sending resumes (`continueSending` on `SENT` or `QCONT`, `:1197-1199`, `:1291-1295`), an owner whose own stored roster is at `link_roster_version` with the column set enqueues an `LSResign` sync.

### 4.11 Subscriber count

In multi-owner mode the owner branch of `updatePublicGroupData` (`Internal.hs:1618-1631`) recounts and, when `members_catchup = 0` in the main app, schedules a publish: the first trigger inserts an entry in `ChatController.countPublishPending :: TMap GroupId ()` and forks a thread that sleeps `channelCountPublishInterval` (60 s), deletes it and enqueues a link sync. The fork is a delay, bounding count writes that would make other owners' swaps conflict in a burst of joins. The relay-announcement threshold becomes `publicMemberCount > number of current owners` (`Subscriber.hs:1445-1449`).

### 4.12 Profile edits and the link counter

- In multi-owner mode `runUpdateGroupProfile` (`Commands.hs:4164-4192`, behind every profile command) calls `writeChannelLink` with `CLCProfile base new`, `base` being the local profile at the command's start, compared as `J.toJSON` values; the callback writes the local profile and sends `XGrpInfo` with the written `linkVersion`. The API is unchanged.
- The apps' profile editors refuse to save, with the "changed by another owner" alert, when the model's channel profile changed while the editor was open (§6.1; an `XGrpInfo` from another owner updates the model). The core check covers a change that reached the link but not this device.
- `xGrpInfo` (`Subscriber.hs:3840-3865`), on relays and members alike, gates locally, with no link read: an event whose `linkVersion` (absent = 0) is below `link_data_version` is not applied; a higher one is applied and stored. Relays forward a current owner's `XGrpInfo` whatever their own gate decides. A channel that never had a swap has no version and behaves as today.

### 4.13 Relays in a multi-owner channel

- **Owner events** as today, plus the commit (§4.6); relays apply the link at invitation and in relay checks (§4.4).
- **Departed owners.** A relay that learns of an owner's departure from a read keeps its connection until that owner's `x.grp.leave` or a remover's `x.grp.mem.del`, since its last blob can be queued on it. From that connection it processes only `x.grp.leave`, roster transfers (`xGrpRoster`, `:3503-3540`, and their chunks, `bFileChunkGroup`, `:2626-2633`) and versioned role and removal events, up to `link_roster_version`, which the read that taught the departure set at or above the owner's last claim; the direct path has no other sender-status check (`Store/Connections.hs:110-112`).
- **Activation.** The `LINK` handler body (`Subscriber.hs:1437-1477`) becomes `activateGroupRelays :: User -> GroupInfoKeys -> [ShortLinkContact] -> CM ()`, called by the handler and the `CLCRelays` callback; it reads `group_relays` and the group link row only when present. `CEvtGroupLinkDataUpdated` and `CRGroupRelaysAdded` carry `Maybe GroupLink` (`Controller.hs:862`, `:1035`).
- **Publication** after a relay's acknowledgement (`Subscriber.hs:3661-3665`) and a pre-roster relay's connection (`:946-947`): a link sync (§4.4).
- **Removal** of a relay (`Commands.hs:3149-3153`): `writeChannelLink` with `CLCRelays [] [link]`; a relay's own leave seen by any owner: a link sync.
- **`APIAddGroupRelays`** without a group link row takes the link from `publicGroup.groupLink` (`Commands.hs:2790-2793`). A relay below `multiOwnerVersion` stays invited (its `updateRelayGroupKeys` fails); the owner removes it.
- **Owners join relays added by others** from `XGrpRelayNew` (`Subscriber.hs:3881-3886`) and on reads. The relay's `memberJoinRequestViaRelay` (`:1690-1720`) accepts a key-proven join onto a current owner row, as for roster rows, never onto a departed one; on the owner, `updatePreparedUserAndHostMembers'` (`Store/Groups.hs:821-844`) and the host `CON` branch (`Subscriber.hs:892-922`) leave an owner membership, the group profile and joined items alone.
- **Invitation by any owner.** `updateRelayGroupKeys` (`Store/Groups.hs:2353-2373`) creates a row (`createLinkOwnerMember`) for every chain owner without one and applies `revokedOwners`, which the inviting-owner check (`Subscriber.hs:4598`) excludes.
- **Capabilities.** `XGrpRelayCap` (`Subscriber.hs:1103-1106`) is stored whatever the receiver's role (it can precede the commit), without this device's `group_relays` row in `group_members.relay_web_domain` (`relayWebDomain` in JSON; `updateRelayCapabilities`, `Store/Groups.hs:1901-1911`). A relay sends it to an owner on its commit or join (`sendRelayCapIfNeeded`, `Internal.hs:2686-2696`, sends only on a domain change).

### 4.14 Other guards

- **Durable rows.** In channels `getGroupMembersForExpiration` and `getRemovedMembersToCleanup` (`Store/Groups.hs:1357-1382`) skip owner and roster-role rows; an intro (`xGrpMemIntro`, `Subscriber.hs:3320-3322`) never changes a departed member's status.
- **Group-link commands** `APICreateGroupLink`, `APIAddGroupShortLink`, `APIDeleteGroupLink` refused for channels (`Commands.hs:3349-3387`).
- **`APIShareChatMsgContent`** signs with any current owner's member key (`Commands.hs:1238-1240`).
- **Connect plan and join** exclude `revokedOwners` from `glOwners`, `verifyLinkOwner` and owner binding (`Commands.hs:4615-4653`, `:4806-4815`); `APIConnectPreparedGroup` creates owners for non-revoked entries, departed rows for revoked ones (`:2324-2332`).
- **History for joiners.** A signed as-channel item by an author no longer a current owner is served unsigned (`FwdChannel`, `itemForwardMsgs`, `Internal.hs:1540-1548`).
- **Destroying.** A single-owner creator first reads the link as for leaving (§4.9); a failed read lets its destroy go on as today. In multi-owner mode `APIDeleteChat` by an owner (`Commands.hs:1384-1401`) first reads (`getConnShortLinkForWrite`) and applies the link: a departed device completes its departure and sends no `x.grp.del`; `AUTH` (no link: this device's earlier delete landed) goes on to `x.grp.del`; any other failure refuses. Every owner, the creator included, then deletes the link queue with `deleteLinkQueueWithCreds` before sending `x.grp.del`, so the link server orders a destroy against a concurrent removal of its owner; on failure it returns the error and deletes nothing. The creator then runs `deleteGroupLinkIfExists` for its local records.

### 4.15 Main app and chat start

`ChatController.mainApp :: TVar Bool`, set by `startChatController` (`Commands.hs:232-269`). Syncs, count timers and catch-up run only in the main app; the iOS notification extension resumes no agent commands (`Commands.hs:255`, `:336`), and an event it processes leaves its trigger in a row. Main-app chat start (`startChannelOwnerWork`) and `APIActivateChat` with `restoreChat` (`:599-610`, iOS's return to the foreground) enqueue an `LSResign` sync per multi-owner channel where the user is a current owner, covering every pending kind of §4.4, and restart a pending catch-up.

### 4.16 Types, database, configuration

| Kind | Name | Notes |
|---|---|---|
| commands | `APIInviteGroupOwner`, `APIAcceptGroupOwnerInvitation` (§4.6) | record syntax and Bot API docs, as every command |
| error | `CEChannelLink {channelLinkError :: ChannelLinkError}` | `CLENotVerified`, `CLERelayVersion`, `CLELinkServerVersion`, `CLEDirectConnection {host, prohibited}`, `CLEChangedByOwner`, `CLERosterSyncing`, `CLELastOwner`, `CLENotOwner`, `CLEOwnerLimit`, `CLELinkDataTooLarge`, `CLELinkBusy`, `CLERelaysUnreachable` |
| item | `RGEOwnerInvitation {invStatus :: OwnerInvitationStatus}` | `OISPending`, `OISAccepted` |
| API field | `canAddOwner :: Maybe Bool` in the group member info response (`APIGroupMemberInfo`), not in `GroupInfo` | computed there from owner rows (current owners, plus one while the creator is an owner) as §4.6 counts (S7); optional in JSON, the apps treating it as absent = allowed, the core still refusing; the role picker's disabled Owner (§6.1) |
| command function | `CFChannelLinkSync`, `CFChannelLinkResign`, `CFChannelLinkSwap` | text `channel_link_sync`, `channel_link_resign`, `channel_link_swap` |
| columns on `groups` | `link_data_version INTEGER`; `link_roster_version INTEGER`, `link_roster_seen_at TEXT` (`TIMESTAMPTZ`); `members_catchup INTEGER NOT NULL DEFAULT 0`; `roster_pending_msg_id INTEGER`; `link_owner_data BLOB` (`BYTEA`) | highest `linkVersion` read, written or applied; last roster commitment and when it first appeared; catch-up pending; own roster's last chunk not yet `SENT`; an owner's stored copy (§4.3) |
| columns on `group_members` | `owner_inv_msg_id BLOB` (`BYTEA`), `owner_inv_accepted INTEGER NOT NULL DEFAULT 0`; `relay_web_domain TEXT` | promoter's pending invitation, accepted; relay capability without the relay's row |
| config | `channelLinkWriteAttempts = 5`, `channelCountPublishInterval = 60 s`, `rosterAbandonTTL = 1 day`, `rosterRequestTimeout = 5 min`, `memberCatchupTimeout = 1 min`, `channelOwnerWaitTimeout = 30 s` | `ChatConfig` |
| hook | `channelLinkWriteHook :: Maybe (GroupInfo -> IO ())` | `ChatHooks`, between taking link data (the copy, a read or a swap's reply) and applying it: in a user write and at channel open outside the group lock, in a link sync on the event thread; tests |
| state | `linkWriters :: TMap GroupId Lock`, `linkSyncSwaps :: TMap GroupId Int`, `countPublishPending :: TMap GroupId ()`, `memberCatchups :: TMap GroupId (Int, Maybe MemberId)`, `rosterRequests :: TMap GroupId UTCTime`, `mainApp :: TVar Bool` | `ChatController` |

One migration `M<date>_channel_multi_owner` with its down migration, dated when S3 adds it so that it sorts after the newest migration on its base branch and on master (`M20261018_autoincrement_ids` and `M20261021_badge_subscriptions` now), SQLite and Postgres, registered in both `Migrations.hs` and `simplex-chat.cabal`; schema dumps regenerate from tests.

### 4.17 Races and where each is ordered

| Race | Ordered by (where) | Outcome |
|---|---|---|
| Two link writes of any kind | the swap (§4.1) | the second re-applies to the current data its refused swap brings back, or is refused |
| O1 promotes while O2 removes O1 | the swap: keys and data together (§4.4) | removal first: O1 reads itself revoked; else M's key is kept |
| Mutual removal; a removal and a leave; the last two leaving | the swap, last-owner check per attempt (§4.8, §4.9) | one current owner remains |
| Two role changes, one with several deltas | `roster.version`, claims from the complete roster (§4.10) | the second waits for the first's blob (`CLERosterSyncing`), then claims `v + 2`; refused for the same target |
| A destroy and a removal of its owner | the swap and `DEL` (§4.14) | whichever lands first; the other fails |
| A role change just before leaving, or in flight when its author is removed | the claim; the leaver's re-sign and wait (§4.9, §4.10) | departed authors' roster messages accepted; a blob no relay got is abandoned after a day |
| Two profile edits | the editor's check, the swap, `linkVersion` (§4.12) | the later refused if the profile changed; devices keep the newer |
| A relay added while another is removed | the swap (§4.13) | both kept |
| A member removed (by an owner or an admin) or leaving while promoted | the link (§4.4 step 4, §4.8) | the promotion wins; if the member's device applied the removal or left first, it stays listed as an owner until an owner removes it |
| A user's role change and a sync on one device | the link writer lock (§4.10 rule (a)) | no claim taken for a lost one |
| A single-owner write queued before the creator's first swap | `LSET` refused in the store on a multi-key queue (§4.1) | `AUTH` |

## 5. Compatibility and security

- **Wire.** SMP v24 (`LSWP` and its `LCUR` reply, `LSET` refused on multi-key queues); a v23 server gets no `LSWP`. Chat v23 events (§4.5); `XGrpInfo` gains an optional field. Two API commands, one response and new types, in the Bot API docs. Internally the agent's `LDATA` gains two fields, `AEvent` gains `LSWAP`, and its command store a constructor.
- **Database.** One chat migration, columns only, both backends; existing rows get NULL or 0. No agent or server schema change. A downgraded app drops the stored copy with the other columns; an upgraded one reads first again.
- **Downgrades.** An older server skips `LKEYS` lines and restores keys from the `KEYS` lines; it loses those swaps' data since its last start and accepts `LSET` again (multi-owner writes stop: there is no `LSWP`); readers ignore its older data until the next write, which takes the version past theirs. An older creator's `LSET` on the multi-key queue gets `AUTH`. An older agent marks stored `LSWP` commands failed and keeps them, with their keys, in its command table.
- **Older parties.** Promotion needs every relay at chat v23 and the link server at SMP v24 (link servers, presets included, upgrade first). Older members ignore `x.grp.owner.add` and reject the new owner's signed messages until they upgrade; older joiners take every chain entry as an owner, including revoked ones, whose keys authorise nothing on the link queue.
- **Adversary.**
  - Relay: cannot forge owner-signed events, commits, acceptances or removals; can withhold or reorder as today (link reads and the roster gap request converge); can replay old signed events, which the roster gate, the `linkVersion` gate and departed rows stop; cannot substitute a promoted member's key (verified code); can fabricate subscribers in a page, never privileged ones.
  - Link server: cannot forge link data (a refused swap's data is verified as a read is); a stale read or reply is ignored by `linkVersion` and otherwise only adds owners, so a party can take a removed owner as current until its next read (an honest removed device sends nothing); ignoring the compare-and-swap brings back lost writes and roster divergence, no forgery.
  - Owners and removed owners are trusted (non-goal). The link orders profile edits, roster role changes and removals, owner additions and removals, leaving, destroying and the relay list: honest software sends these only after its link write lands (destroy: after its read and `DEL`). Posts, moderation, blocks, subscriber removals, relay invitations and owner invitations are sent as today. Until a relay learns of a removal (the remover's announcement, or its link check every 15 minutes), a removed device running modified software can act there as an owner, including role events at inflated versions that stop later role changes at that relay and its members; afterwards the relay accepts from it only its departure, roster transfers and role events up to the link's roster version, new ones at that version included, which members accept above their gate. With a colluding link server it can serve forged link data to readers.
  - Any current owner can destroy the channel or replace the key set with `RKEY`.
- **Privacy.** A promoted owner connects directly to the link server (recipient commands are not proxied), with the consent and refusals of §4.6; link commands use a session of their own, link reads stay proxied. Owners write from their stored copy, so they don't read before every write; they read at channel open, at chat start and in the other cases §4.3 lists, routed as other link reads are. The current data a refused write needs comes back in its reply over the direct link session, where the server already sees the write, and the server learns nothing it did not store. The copy is link data any subscriber can read, kept in the encrypted chat database. The link server learns when owners act and how many there are; the member-page cursor is a memberId. A pending async swap keeps the owner's keys in the agent's command table (the encrypted database of its queue keys) until it completes or its mailbox goes.

## 6. Slices

Each slice compiles and passes its tests on its own; a slice with numbered parts makes one commit per part. simplexmq: `cabal test simplexmq-test --test-options='-m "<pattern>"'` (Postgres with `-fserver_postgres`); chat: `cabal test simplex-chat-test --test-options='-m "channels"'` plus the slice's own patterns; S3 builds on the pinned simplexmq, S4 and S5 with `use-local-simplexmq` until S6 pins it. Swift and Kotlin are built and tested by the app test workflows on GitHub's runners (§7.1). Hours cover implementing a slice with its tests, building, testing and reviewing it, estimated from earlier slices of this codebase (a chat library rebuild takes 30–70 minutes; a slice of about 130 lines took 4.3 hours) and, for app slices, from the CI READMEs' run times.

| # | Slice | Hours | Depends on |
|---|---|---|---|
| S1 | simplexmq: the store's compare-and-swap, `LSWP` and SMP v24 | 7.5 | simplexmq's `f/channel-multi-owner` branch, started from `8bc4d360` |
| S2 | simplexmq agent | 5.5 | S1 |
| S3 | chat wire and data, on the pinned simplexmq | 6.0 | nothing: a branch off `f/channel-multi-owner`, beside S1–S2 |
| S4 | chat on the new simplexmq; link writes | 8.0 | S2, S3 |
| S5 | link syncs; callers | 7.5 | S4 |
| S6 | CI: simplexmq pinned, a test backend with channels on a v24 link server, the Linux Claude jobs ready for it | 6.5 | S4's first part; simplexmq's branch pushed, so the pin and its `scripts/nix/sha256map.nix` entry exist |
| S7 | promotion | 8.5 | S5 |
| S8 | member catch-up; guards; T17 | 6.5 | S7 |
| S9 | roster claims | 7.5 | S8 |
| S10 | roster background work | 5.0 | S9 |
| S11 | iOS: owner invitation and promotion; the backend's owner automation | 8.5 | S6, S8 |
| S12 | Kotlin: the same as S11, with the desktop and Android scripted owner checks | 6.5 | S11 |
| S13 | leave, with re-sign and wait | 8.0 | S10 |
| S14 | removal and destroy | 6.0 | S13 |
| S15 | iOS: owner management screens; the iOS scripted owner check | 8.5 | S11, S14 |
| S16 | Kotlin: the same as S15; every platform's whole plan | 6.5 | S12, S15 |

Sixteen slices, about 112 hours. S3 needs nothing from simplexmq, so it runs beside S1–S2 in another workspace and merges into `f/channel-multi-owner` before S4. The roster slices (S9–S10) come before the first app slices, so the riskiest assumption (§8) is mostly tested before any app work; its cases with a departed owner (T9, T15) come with leaving, in S13.

### What each slice holds

| # | Holds | Tests | Risk |
|---|---|---|---|
| S1 | (1) store: `LinkDataHash`, `swapQueueLinkData` for the stores that exist at `8bc4d360` (STM and Postgres, §4.1), the multi-key `LSET` refusal inside the store, `CreateLinkKeys` + `UpdateKeys` logging and replay, ~180; (2) protocol: v24, `LSWP` and its `LCUR` reply, handler, client, protocol doc, ~110 | store, store-log and server tests on both stores (Postgres with `-fserver_postgres`), an `LSET` racing a key-adding swap, 6 keys fit and 7 don't | high (atomicity, store log) |
| S2 | agent: §4.2, sync and async, `OwnerLinkData`, `LinkSwapResult`, `LSWAP`, ~230 | agent tests (§7) | high |
| S3 | wire and data (inert), needing nothing from simplexmq: migration (named with S3's own date, after the newest migration on its base branch and on master, in both backends; `link_owner_data` is only a column until S4), link fields, events, versions, errors, the two read command functions (`CFChannelLinkSwap` comes with S4, since its `LSWAP_` is S2's), commands in record syntax (refusing), their view and CLI output, optional `groupLink` in `CEvtGroupLinkDataUpdated` and `CRGroupRelaysAdded`, config, hook, state; Bot API docs, ~300 | `-m "channels"`, protocol, JSON, "Bot API docs", "Schema dump" | medium (migration) |
| S4 | (1) structural, chat on the new simplexmq: the `LDATA` match, the `LSWAP` event passed by, `CFChannelLinkSwap`, and the plan's simplex-chat line references re-anchored in a docs commit; extract `activateGroupRelays` and the channel intro member creation, ~90, no behaviour change; (2) link writes: locks, the stored copy, `writeChannelLink`, `continueChannelLink`, `applyChannelLinkData`, `currentOwner`, multi-owner mode, `linkVersion`, `XGrpInfo` gating, durable rows, intro guard, relay reads and tolerant invitation, ~230 | existing tests; T23 | high (every later slice builds on it) |
| S5 | (1) link syncs with no pending kinds yet: mailbox, continuations, `LDATA`/`LSWAP`/`ERR` dispatch, coalescing, refusal bound, main-app gating, chat start, ~170; (2) callers: profile check (core; the editors' rule is in S15–S16), relays (activation, publication, removal, owner joins, capabilities), count publisher and threshold, ~190 | existing tests; single-owner relay and count behaviour unchanged | high (concurrency) |
| S6 | (1) simplexmq pinned to S1–S2's pushed commit (`cabal.project`, `scripts/nix/sha256map.nix`); (2) the app test harness changes master doesn't have yet: the `ui` fixes `5f6030568` (closes the desktop `ui key … exec` escape to the token; defines desktop's `number`) and then `c229dcd2c` (no file name a Claude session creates can run a command), iOS private routing set to unknown servers for backend runs (master's default became "always"), the desktop Kotlin compile heap at 4 GB, `ui backend …` on desktop and Android as iOS has; (3) the feature's test backend, ~350 lines: in both `backend.sh` files (Android uses desktop's) a second relay; an owner peer `ci_owner` with storage on the runner's own SMP server (v24 once pinned) creating `ci_owners_a` and `ci_owners_b` with both relays; a subscriber peer `ci_second`; every peer that reaches that server (`ci_owner`, both relays, `ci_second`) has it among its servers or routes with proxy mode never; `ci_owner` and `ci_second` kept online (the CLI's `-p` websocket API, or a polling loop); the channels' links in `seed.md`; a self-check that a fresh peer joins each channel through both relays; the desktop and Android Claude jobs given a backend prompt that includes `seed.md`, the app started at the chat list with the runner's server and routing set, about 200 turns and a job timeout to match | runs: `desktop-linux`, `android-emu` and `ios-sim` with `backend`, scripted; `desktop-linux` once more with `claude` and `backend`, its backend prompt holding only a smoke check of the seed, to prove the new start state | medium (new backend parts, the token boundary) |
| S7 | promotion: invite, accept with the direct-connection check, `CLCAddOwner` in syncs with the creator's first-swap fields, commit, M becomes owner, ~220 | T1 (without the role change), T2, T5 (profile, relays), T12, T13, T20 | high (first end-to-end flow) |
| S8 | (1) member catch-up with watchdog, ~120; (2) guards: group-link commands, share signing, connect plan, history, ~60 | T10; T17 (the background promotion's restarts and outages, idempotent callbacks), with (1); existing tests for (2) (the history check comes with T3 in S13, which first makes a departed owner) | high (T17) |
| S9 | roster claims: claim from the complete roster, owners' early requests, owners outside the roster, stored-blob rules, departed authors on owners, members and relays, relays serving owners, ~160 | T1 role change, T5 roles, T21 | high (the riskiest assumption) |
| S10 | roster background work: sync rules (a)–(c), abandonment, the undelivered-roster column and triggers, ~90 | T22 (without the leave case) | high |
| S11 | (1) iOS, §6.1 rows marked S11, ~200 Swift; (2) the backend's owner automation in both `backend.sh` files (Android uses desktop's), ~150 lines: `ui owners invite a\|b` (`ci_owner` reads the `group_member_id` of that channel's newest member other than `ci_second`, the app's user, with `sqlite3`, verifies its code and invites it with the `/_` id forms only, never a name, and completes the promotion when it accepts), `ci_second` accepting owner invitations with `direct=on`, `ui owners profile a\|b` (`ci_owner` sets the next of a few fixed profiles); the subcommands take only `a` or `b`, never text; the iOS plan's promotion cases and `claude-scope` (backend yes, the cases, turn and budget caps) | runs: `ios-sim` with `claude` and `backend` | high (first app run of the flow, the token boundary) |
| S12 | Kotlin, the same rows, ~200; the scripted owner check in the desktop and Android `smoke-test.sh`; the promotion cases in their backend prompt | runs: `desktop-linux` and `android-emu` with `claude` and `backend` | medium |
| S13 | leave (§4.9) with re-sign and wait, and the departure that the removed device reuses, ~90 | T3 (without the destroy, with the history check), T7, T9, T11, T14, T15, T16, T22 leave case | high |
| S14 | removal: command, receivers, removed device (§4.8); destroying (§4.14), ~80 | T4, T6, T8, T18, T19, T3's destroy step | high |
| S15 | iOS, §6.1 rows marked S15, ~180; the iOS plan's owner-management cases; iOS `smoke-test.sh` gets a case argument as desktop and Android have (the app job passes `backend`, the Claude job's pre-session run passes none, so the check never promotes the Claude session's user) and the scripted owner check, ~150 CI lines | runs: `ios-sim` with `claude` and `backend` | medium |
| S16 | Kotlin, the same rows, ~180; the desktop and Android prompts get the owner-management cases; then each platform's whole plan once (§7.1) | runs: `desktop-linux` and `android-emu` with `claude` and `backend`; `ios-sim` again only if the core changed after S15's run | medium |

### 6.1 Apps (iOS and Kotlin, mirrored)

Same file split, names and branch order on both platforms. iOS goes first (S11, S15) and Kotlin mirrors it in the next slice (S12, S16); each pair is tested on GitHub's runners (§7.1).

| Change | iOS | Kotlin | Slices |
|---|---|---|---|
| API: `apiInviteGroupOwner`, `apiAcceptGroupOwnerInvitation(allowDirect)`, `ownerInvitationSent`; `groupLink` optional in `groupRelaysAdded`, `groupLinkDataUpdated`, `AddGroupRelaysResult.added` | `SimpleXAPI.swift`, `AppAPITypes.swift:1001, 1210` | `SimpleXAPI.kt:6815, 6846` | S11, S12 |
| Errors: `channelLink(ChannelLinkError)`, one alert per case | `APITypes.swift:744+` | `SimpleXAPI.kt:7807+` | S11, S12 |
| `RcvGroupEvent.ownerInvitation(invStatus)`: text, notification, bold like `newMemberPendingReview`; a pending one asks "Become an owner?" (the profile goes to all subscribers; the device connects directly to <host>, revealing its IP address) → accept; on `directConnection` confirm and repeat with `allowDirect`, or when `prohibited` explain the setting | `ChatTypes.swift:6054+`, `ChatItemView.swift:153-157` | `ChatModel.kt:5447+`, `ChatItemView.kt:734-748` | S11, S12 |
| Role picker: Owner for a current owner and a current non-owner member with a key, while another owner fits (`canAddOwner` in the member info response, §4.16); when none fits, Owner shown disabled with the limit that applies now ("This channel can have at most 5 owners", or 6), with a preview of that state; unverified → verification (`APIVerifyGroupMember`); else confirm (member is asked; owners can remove owners and delete the channel) → `apiInviteGroupOwner`; no picker for owner targets | `ChatTypes.swift:3382-3394`, `GroupMemberInfoView.swift:327-331, 731-768` | `ChatModel.kt:2944-2955`, `GroupMemberInfoView.kt:149-158, 947-991` (roles list keyed to recompute) | S11, S12 |
| Remove: "Remove owner" label and confirmation | `GroupMemberInfoView.swift:697-707`, `GroupChatInfoView.swift:995-1044` | `GroupMemberInfoView.kt:253-334, 843-854`, `GroupChatInfoView.kt:278+` (`removeMemberAlert`) | S15, S16 |
| Leave in the channel info view when another current owner exists; core refusals shown (the chat list keeps today's rule: it has no members loaded) | `GroupChatInfoView.swift:249-254`, `SimpleXAPI.swift:2047-2060` | `GroupChatInfoView.kt:828-838`, `SimpleXAPI.kt:2493-2498` | S15, S16 |
| Link screen: an owner without a `GroupLink` sees the published link; never creates a link for a channel; SimpleX-name setting for any current owner | `GroupChatInfoView.swift:113-127, 162-170, 402-403`, `GroupLinkView.swift:164-166` | `GroupChatInfoView.kt:599-611, 642-653, 670-681`, `GroupLinkView.kt:119-123` | S15, S16 |
| Channel open: owners also call `apiGetUpdatedGroupLinkData` | `ChatView.swift:762-780` | `ChatView.kt:225-242` | S11, S12 |
| Profile editors keep the channel profile they opened with and, on save, refuse with the "changed by another owner" alert if the model's profile differs | `GroupProfileView.swift`, `GroupWelcomeView.swift:145`, `GroupPreferencesView.swift:227`, `GroupChatInfoView.swift:1143`, `ChannelWebAccessView.swift:157` | `GroupProfileView.kt`, `WelcomeMessageView.kt:48`, `GroupPreferences.kt:49`, `MemberAdmission.kt:40`, `ChannelWebPageView.kt:59` | S15, S16 |
| Composer: an owner without `group_relays` rows uses the relay-member state | `ComposeView.swift:739-773` | `ComposeView.kt:2151-2177` | S15, S16 |
| Relays: add-relay excludes relay members' links too; web settings take domains from rows and relay members' `relayWebDomain` | `ChannelRelaysView.swift:38-41`, `ChannelWebAccessView.swift:86-120` | `ChannelRelaysView.kt:109-137`, `ChannelWebPageView.kt` | S15, S16 |
| Strings | `en.xliff` via `NSLocalizedString` | `moko-resources/base/strings.xml` | each with its rows |

## 7. Test plan

**simplexmq** (`tests/ServerTests.hs` per store, `tests/CoreTests/StoreLogTests.hs`, `tests/CoreTests/MsgStoreTests.hs`, `tests/AgentTests/FunctionalAPITests.hs`):

- `LSWP`: right hash replaces user data and keeps fixed data; wrong hash `LCUR` with the stored user data, nothing changed; another link ID or fixed data `AUTH`; no link `AUTH`; messaging queue `AUTH`; via proxy `CMD PROHIBITED`; v23 session cannot send it; 6 Ed25519 keys fit with the fixed and user data at their padded sizes, 7 don't.
- With keys: the removed key gets `AUTH`, the added key writes; `LSET` on a two-key queue, and one racing a key-adding swap, get `AUTH`; concurrent swaps: one `OK`, one `LCUR`, then success (Postgres-only store included).
- Restart: two key-changing swaps replay data and keys; a data-only swap replays as `LINK`; a log without `LKEYS` lines still restores the keys.
- Agent: `LDATA` and the sync read carry the fixed data and hash the swaps accept; root- and owner-signed swaps verify in `getConnShortLink`; a removed signer gets `AUTH`; a swap returns `LSRSwapped` with the data it wrote and a hash the next swap accepts, or `LSRChanged` with the current data, verified (a forged reply fails); a link-data tail and an owner entry's extension written by a newer agent survive another owner's swap; the async swap's `LSWAP` does the same, retries while the server is down, resumes after a restart, and a retry after a landed attempt returns `LSRChanged` with its own data; `deleteLinkQueueWithCreds`; `getConnLinkQueueCreds`; `linkServerSupportsSwap` false on v23.

**chat**, `describe "multiple owners"` under `describe "channels"` (`tests/ChatTests/Groups.hs:285`), relays as in `createChannel2Relays` (`:9783`):

| # | Test |
|---|---|
| T1 | verify, invite, accept; an earlier subscriber accepts M's signed post; a member that knew M by a relay-asserted key ends with the chain key; M edits the profile and changes a role |
| T2 | refusals: unverified; a relay below v23; a link server at SMP v23; a full key set (the creator and four others; after the creator leaves, six), while a full set with the largest allowed profile still writes; M ignores a chain entry with another key |
| T3 | the creator promotes M and leaves; M posts through every relay, edits the profile, adds and removes a relay, promotes a verified subscriber, destroys the channel; a later joiner sees M as owner, the creator departed, verifies the roster, gets the creator's history unsigned |
| T4 | O1 removes O2: O2 applies it on receipt and erases its keys, its next write fails; relays, members and a new joiner treat O2 as departed |
| T5 | the hook holds O1 between taking its link data and its swap: O2's profile edit lands, O1's is refused, as is O1's edit before O2's `XGrpInfo` reaches it; O1 adds and O2 removes relays, both kept; O1 and O2 change roles of different members (O1 retrying after `CLERosterSyncing`), all end with both; O2 changes two members in one command and O1, holding one delta, is refused until it has the blob, which it completes from the second relay when the first stops after the header |
| T6 | mutual removal: one owner remains |
| T7 | the last two owners leave: one refused |
| T8 | an owner's removal racing a promotion: O2 removes subscriber M while the hook holds O1's promotion of M; M's device leaves, every other party, O2 included, takes M as owner at its next read, and O1 then removes M as an owner |
| T9 | O1 demotes the only admin and leaves while O2 is offline; O2 returns and gets O1's roster from a relay; members receiving O1's role events after its leave apply them; a joiner gets the roster; the demotion holds |
| T10 | catch-up: earlier subscribers appear on M with keys (it verifies and promotes one); a restart restarts and skips held members; a silent relay is replaced after the timeout, no member skipped |
| T11 | count: after the creator leaves, joins and leaves change the count within the (shortened) interval, from the first chain entry only |
| T12 | a lower or unversioned `XGrpInfo` is not applied by relays and members; a stale read does not revert the profile |
| T13 | single-owner async writes queued before the creator's first swap land after it with `AUTH`; the link keeps the swap's data and M |
| T14 | chain dependency: creator → M → N; M leaves (its entry stays as N's signer); N leaves; writes keep validating |
| T15 | a relay reads the creator's departure before processing its queued blob and still accepts it; a role event from that connection above the link's roster version is refused |
| T16 | a leave stopped after its swap (hook) completes at the next chat start; a departed author's undelivered version is abandoned after the (shortened) TTL and an unanswered request |
| T17 | an acceptance processed while the link server is down promotes once it is back; a restart between a swap and its `LSWAP` sends the commit once; a relay acknowledgement publishes the relay and a join changes the count; a sync whose mailbox was removed is redone at chat start; a server refusing every swap stops the sync at the bound |
| T18 | an admin removes M while the hook holds O1's promotion of M, one relay processing the removal before the commit and the other after: the first relay, its members and the admin's device take M as owner at their next read, the second relay refuses the removal, and M, receiving the commit first, stays an owner |
| T19 | a device that left after its swap destroys without `x.grp.del`; the creator's destroy is refused while its read fails; a destroy racing a removal of its owner: one fails |
| T20 | a channel-open read held (hook) across a promotion is applied after the commit: M stays an owner everywhere |
| T21 | O2 adds a relay right after O1's role change, and O1 one right after its own: each relay gets the current blob, acknowledges and is published |
| T22 | relays not ready when O1 demotes an admin: O1's chat start, and a relay connection becoming ready, re-sign at the next version and members fetch it; the same with O1 leaving (re-sign, wait, leave) |
| T23 | the stored copy: O1 writes from it with no read; after O2's write, O1's next write is refused with the current data and lands on its second swap, with no read; a swap whose reply is lost (the agent's retry) is found already applied and its callback runs once, a roster claim's included; a new owner's first write and chat start read first; refusals found on a stale copy are confirmed by a read: O1 edits the profile after applying O2's `XGrpInfo` (lands), re-sets its earlier profile after O2's edit (lands, and receivers apply it), and leaves after a promotion its copy predates (allowed) |

Existing tests that must stay green: the whole `-m "channels"` group, `ProtocolTests`, `JSONTests`, "Bot API docs", schema dump. Final: `cabal test simplex-chat-test` (which also regenerates the query plans) and `simplexmq-test`, each also with Postgres (`-fclient_postgres`, `-fserver_postgres`).

### 7.1 Apps on GitHub's runners

The app test workflows (`scripts/ci/README.md`) build the core and the apps from the pushed branch and drive the apps with scripted checks and Claude sessions; they are the build and the test of S11, S12, S15 and S16, and S6 prepares them.

**Test backend.** S6 adds, on the runner: a second chat relay; `ci_owner`, whose channels `ci_owners_a` and `ci_owners_b` have their link on the runner's own SMP server, built from the pinned simplexmq and so at SMP v24; `ci_second`, a subscriber of both; both peers online for the whole run. Every peer that reaches that server has it among its servers or routes with proxy mode never. S11 adds `ui owners invite a|b` (`ci_owner` verifies the code of that channel's newest member other than `ci_second`, which is the app's user, invites it, and completes the promotion when it accepts), `ci_second` accepting owner invitations with `direct=on`, and `ui owners profile a|b` (`ci_owner` sets the next of a few fixed profiles). These subcommands take only `a` or `b`: the Claude session's Bash shares the boundary with its token, so no text it chooses reaches a shell or a CLI command. The app joins from the links in the seed and reaches the runner's server directly: it is among the app's servers, with private routing for unknown servers only, since the preset proxies can't reach the runner. The test directory can't list these channels, as it can't reach queues on that server.

**Scripted checks.** One owner check per platform, in its `smoke-test.sh` backend case (desktop and Android in S12; iOS in S15, after its `smoke-test.sh` gets the case argument the others have, so the Claude job's pre-session run doesn't promote the Claude session's user): join `ci_owners_a` from the seed, `backend.sh owners invite a`, open the invitation and accept, wait for the role item, and check that the member info shows Owner. They run on every run, without Claude.

**Cases.** Claude sessions with Haiku follow them: `| ID | Do | Expect |` tables in the iOS plan, the same steps in the desktop and Android backend prompt. Promotion cases come with S11 and S12, owner management with S15 and S16.

| ID | Do | Expect |
|---|---|---|
| O1 | join `ci_owners_a` from its link | subscribed; the channel info shows `ci_owner` as owner |
| O2 | `ui owners invite a`; open the invitation in the chat with admins | "Become an owner?" says the profile goes to all subscribers and names the server the device will connect to directly |
| O3a | set private routing to always, keep the fallback at its default; accept | the explanation that the settings forbid a direct connection; nothing sent |
| O3b | also allow the downgrade to a direct connection; accept; then restore both settings | the direct-connection prompt; after confirming, the acceptance is sent |
| O4 | wait for the promotion | shown as owner, with the role item in the channel; the member list includes `ci_second` |
| O5 | open `ci_second`, choose Owner | the verification screen first; after "Mark verified", the confirmation, then the invitation sent; `ci_second` becomes an owner |
| O6 | open `ci_owner` | no role picker |
| O7 | `ui owners profile a`; reopen the channel | the new profile |
| M1 | open the channel's link screen | the published link, no button that creates a link, the SimpleX-name setting |
| M2 | post in the channel | sent as the channel, no relay error in the composer |
| M3 | open the relays screen, add a relay | the channel's relays aren't offered again |
| M4 | open the profile editor; `ui owners profile a`; save | the "changed by another owner" alert and the current profile |
| M5 | remove `ci_second` | "Remove owner" and its confirmation; `ci_second` shown as removed |
| M6 | leave from the channel info | offered while `ci_owner` is an owner; the user has left |
| M7 | join `ci_owners_b`, `ui owners invite b`, accept; remove `ci_owner`, then open the channel info | no Leave: the user is the last owner |

**Layouts.** One Claude session in the reachable layout runs every case; none renders differently by layout. M7 uses `ci_owners_b`, so that removing `ci_owner` there leaves `ci_owners_a` for the other cases.

**Runs and time** (the CI READMEs). S6 runs `desktop-linux`, `android-emu` and `ios-sim` with `backend`, scripted, and `desktop-linux` once more with `claude` too. Each app slice runs its platforms with `claude` and `backend`. The first run after Haskell changes rebuilds the core and the backend: on iOS 30–45 and 20–45 minutes, plus the wait for a macOS runner (minutes to an hour: the fork's macOS runners are shared by all its runs, and GitHub drops a job no runner picks up in about 15 minutes, which is then started again), the app job, about 10 minutes to start the backend and the Claude sessions (a 50-case plan took 30–60 minutes a Claude session; these have 7–14 cases); on desktop and Android about 25 and 30 minutes, the two workflows in parallel. Reruns reuse both builds. The Claude sessions' turn caps rise from 60 to about 200, and the Linux Claude jobs' timeout with them (S6).

**Not on the runners.** Only the Haskell tests cover the races they force with hooks, a link server that is down, a restart mid-swap and Postgres. Checked by hand on devices: the iOS notification extension leaving owner work to the app (§4.15) and the app's return from the background; promotion in a channel on the public preset servers once they run SMP v24; an older app as a subscriber rejecting a new owner's messages; the web settings taking domains from relay members (a case instead, if S6 can give a test relay a web domain). A 100,000-member catch-up stays an estimate.

## 8. Risks

- **Riskiest assumption:** members' and relays' unchanged roster protocol converges when versions are claimed through the link from complete rosters and departed owners' roster messages pass the gate (T5, T21 in S9; T22 in S10; T9, T15 in S13). The roster slices come before the first app slices, so it is mostly tested before any app work; the departed-owner cases (T9, T15) come with leaving in S13, after the first app slices. S1 first proves the swap's atomicity and size.
- Background syncs re-derive every callback from the latest link data and rows; a non-idempotent condition would send twice or never (T17 in S8).
- **The test backend's link server is the runner's own SMP server.** A peer or an app that reaches it through a preset proxy gets nothing. S6 has every backend peer join both channels before an app slice depends on them, and the cases keep private routing at unknown servers except in O3.
- **Android's Nix core build must accept the pinned simplexmq** (its `scripts/nix/sha256map.nix` entry); S6 runs `android-emu` with the pin first.
- **The Claude session's shell shares a boundary with the CI's Claude token.** The backend subcommands S6 and S11 add take only a channel letter and address members by id, S6 first brings in the `ui` fixes `5f6030568` and then `c229dcd2c`, and what S6 and S11 add to `ui` is checked together with the guard (`claude-guard.py`), which decides what the Claude session may run.

## 9. Open questions

None: every question is decided (overview, Decisions).

## 10. Out of scope

Pre-existing issues: relay forwarding order (`Store/Delivery.hs:178-224`); messages to a backlogged relay dropped (`Internal.hs:2840-2846`); a queued async link write overwriting a newer one (`Agent.hs:1191-1200`); the async `LSET`'s client data lost by its parser (`Agent/Protocol.hs:2322`); admin promotion with relay-reported keys (`Commands.hs:3053`); receivers letting an admin remove an admin in relay groups (`Subscriber.hs:3783-3786`).

## 11. Follow-up runs

- simplexmq's pull request to its master, and simplex-chat re-pinned to the merged commit, before the chat pull request.
