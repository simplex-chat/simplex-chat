# Roster: regular members + larger rosters via inline file

Date: 2026-06-01 (revised). Extends Section 1 of `2026-05-26-public-groups-via-relays-unified.md`.

> Anchors re-verified against the tree. Two known drifts: the `xGrpRoster` block is ~8 lines below the original review numbers, and `encodeChatMessage` is in `Protocol.hs:909` (not `Internal.hs`). Confirm before editing.

## Goal

Let owners promote channel subscribers to **regular members** who can post, and carry more named members than fit one message.

The JSON roster already exists (event, signing, relay cache, TOFU apply, broadcast, join forward, QCONT). This plan **changes only the delivery** and reuses the apply logic (`processRoster`).

The member list moves out of the `XGrpRoster` message into a binary blob sent over the existing inline file transfer. `XGrpRoster` becomes a small signed header (version + the blob's size and digest).

## Roster set: the promoted set {member, mod, admin}

Owners stay on the link, never in the roster. Two edits, then every gate follows.

**1. Widen `isRosterRole`** (`Internal.hs:1230`) to `{GRMember, GRModerator, GRAdmin}`. Every call site wants the promoted set, so this single edit covers:

- `validateGroupRoster` filter (`Internal.hs:1236`) — fixes the bug where member entries are dropped.
- `buildGroupRoster` filter (`Internal.hs:1248`).
- promotion gates / cap / trigger / counts (`Commands.hs:2737, 2739, 2746, 2762, 2763, 2768`); update the cap error text at `2740`.
- owner-remove roster refresh (`Commands.hs:2888`, guarded by `anyPrivilegedRemoved` computed from `isRosterRole` at `2899`) — so removing a plain member, not just a mod/admin, refreshes the roster.
- receive gates: `xGrpMemNew` (`Subscriber.hs:2975/2990/3009`) and `xGrpMemRole` owner-only (`3157`).

**2. Split the role query.** `getGroupRosterMembers` (`Store/Groups.hs:1214`) currently serves two now-diverging needs:

- **Build / revert** wants the promoted set. Redefine `getGroupRosterMembers` to `member_role IN (GRMember, GRModerator, GRAdmin)` (current members). Callers: `bumpAndBroadcastRoster` (`Internal.hs:2151`), `sendGroupRosterToRelay` (`2162`), and the `processRoster` revert set `currentPriv` (`Subscriber.hs:3207`). Build and revert MUST be the same query, or a dropped member is never reverted.
- **`introduceInChannel`** (`Internal.hs:1185`) wants only the moderation set (mod+admin). Widening it would announce every joiner to every member and introduce every member to every joiner (traffic + anonymity blowup). Reuse the existing `getGroupModerators` (`Store/Groups.hs:1203`, returns mod+admin+owner) rather than adding a function: keep `getGroupOwners` for the owner-first intro, and take mod+admin as `getGroupModerators` minus owners (preserving the current-member filter `getGroupRosterMembers` had). Members are learned from the roster blob, not introductions.

**Owner-only (confirmed decision).** Only the owner changes any roster role. The alternatives were considered and rejected for v1 — letting a mod/admin set member roles would need either the owner co-signing rosters from a mod/admin (owner round-trip + load) or a separate roster-signing key trusted from mod/admin (broader trust surface) — so owner-only keeps the single owner-key trust anchor.

**Leave and owner-remove differ.** A member-initiated **leave** (`xGrpLeave`) does NOT bump the roster — that leave-triggered refresh is being removed separately and this plan does not rely on it; the leave is handled on the membership axis (`XGrpLeave` neutralizes the member on the relay). An owner-initiated **remove** (`APIRemoveMembers`) DOES still bump the roster via `bumpAndBroadcastRoster` (`Commands.hs:2888`), so a removed member is dropped from the roster blob for new joiners — and that site is widened (above) to cover plain members, not just mod/admin.

## Wire: signed header + unsigned blob

**Authoritative metadata is in the signed header.** `version`, blob `fileSize`, and `fileDigest` all live in the owner-signed `XGrpRoster`; the unsigned `BFileChunk`s carry no authoritative metadata. (This is why "total parts in the unsigned part", an earlier review question, is a non-issue here.)

- **Header**: `XGrpRoster { version :: VersionRoster, fileInv :: InlineFileInvitation }`, JSON, signed, forwarded. `InlineFileInvitation { fileSize, fileDigest :: FD.FileDigest }` is a lean `FileInvitation` (no name/connReq/inline/descr; always inline). Tiny; fits `maxEncodedMsgLength`.
- **Blob**: the binary member list. `RosterMember { memberId, key, role, privileges :: Word16 }` — drop `name`, add `privileges` (reserved: always `0`, parsed and ignored in v1). ~60 B/entry. Members get a placeholder name from `nameFromMemberId`; real profiles arrive on first post.
- **Serializer/parser**: a binary codec for the blob (a `Word16`-count-prefixed `[RosterMember]`); `RosterMember` becomes binary-only. Full code in *Blob format* below. Owner serializes → digest → chunks; receiver concatenates chunk bytes → verifies the digest → parses.
- **Cap** `maxGroupRosterSize` → **256** (tunable). Enforce at promotion over the promoted set (`Commands.hs:2739`, via the widened predicate); re-validate the parsed entry count on receive (`Subscriber.hs:3171`); reject a signed `fileSize > cap × max-entry-size` before creating a file. Roster files are exempt from the inline `offer/receiveChunks` ceiling (at 256 ≈ 15 KB the blob is about one `fileChunkSize` chunk; the multipart path handles two if role words push it over).

### Blob format (serializer / parser)

`RosterMember` is now **binary-only** — carried in the blob, never in a JSON message — so remove its `deriveJSON` (`Protocol.hs:806`) and give it the `Encoding` below. `MemberKey` (`Types.hs:972`, only `StrEncoding`) and `GroupMemberRole` (`Types/Shared.hs:33`, only `TextEncoding`) lack a binary `Encoding`: `MemberKey` delegates to the underlying `PublicKey` (`Crypto.hs:568`), and the role delegates to its canonical `TextEncoding` (the same `"member"/"moderator"/"admin"` form JSON and the DB use — single source of truth; `GRUnknown` round-trips).

```haskell
-- MemberKey gains a binary Encoding (it only had StrEncoding); delegate to the Ed25519 key.
instance Encoding MemberKey where
  smpEncode (MemberKey k) = smpEncode k
  smpP = MemberKey <$> smpP

-- Roles encode as their canonical text (the form JSON and the DB already use), not an ad-hoc
-- numeric tag — single source of truth, and GRUnknown round-trips. textDecode returns Maybe
-- (not Either), so the <$?> operator (Either-only, Util.hs:115) does NOT apply — use the maybe form.
instance Encoding GroupMemberRole where
  smpEncode = smpEncode . textEncode
  smpP = maybe (fail "roster: bad role") pure . textDecode =<< smpP

instance Encoding RosterMember where
  smpEncode RosterMember {memberId, key, role, privileges} =
    smpEncode memberId <> smpEncode key <> smpEncode role <> smpEncode privileges
  smpP = do
    memberId <- smpP
    key <- smpP
    role <- smpP
    privileges <- smpP
    pure RosterMember {memberId, key, role, privileges}

-- Blob = Word16 count (NOT smpEncodeList: its 1-byte count overflows at the 256 cap) followed
-- by that many entries. This is the byte sequence the digest is computed over and verified
-- against before parsing.
encodeRosterBlob :: [RosterMember] -> ByteString
encodeRosterBlob ms = smpEncode (fromIntegral (length ms) :: Word16) <> B.concat (map smpEncode ms)

rosterBlobP :: Parser [RosterMember]
rosterBlobP = do
  n <- fromIntegral <$> smpP @Word16
  when (n > maxGroupRosterSize) $ fail "roster: too many entries"
  A.count n smpP
```

- **Owner**: `encodeRosterBlob` over the promoted set → SHA-256 digest → chunk; the digest goes in the signed `XGrpRoster` header.
- **Receiver**: concatenate chunk bytes → verify the digest (S1, over plaintext) → `parseAll rosterBlobP` (consume all input; reject trailing bytes). Parsing runs only after the digest matches, so the bytes are owner-attested; the `n > maxGroupRosterSize` guard and `parseAll` are defensive against a buggy/garbled blob.
- **Per-entry layout**: `memberId` (1-byte len + id) + `key` (1-byte len + Ed25519 pubkey) + role (1-byte len + role word, e.g. `member` = 7 B) + `privileges` (2 bytes) ≈ ~60 B/entry. The file-transferred blob has no tight size budget, so canonical text is fine.
- `privileges` is reserved: serialized as `0`, parsed and ignored in v1.

## Delivery: send → header → chunks → completion

### Owner send

`bumpAndBroadcastRoster` and `sendGroupRosterToRelay` build the blob (`buildGroupRoster` over the widened query), compute its `FileDigest`, send the `XGrpRoster` header, then send the blob as `BFileChunk`s against that message's `shared_msg_id`.

`sendFileInline_` reads from a file, so add a send-from-bytes variant (shared with the relay re-serve). The owner's own version bump stays as today (`Internal.hs:2148/2152`) — the owner is the source of truth; "bump only at completion" is a receive-side rule.

### Header handler (`xGrpRoster`, member and relay)

The header no longer applies anything — it starts a transfer. It only writes `roster_pending_*`; it never writes `roster_version` or the live `roster_msg_*`.

- **Short-circuit** unless `version > max(roster_version, roster_pending_version)` — strictly greater than both applied and pending — before creating a file.
  - Why both: the QCONT re-serve is unconditional, so the relay may re-forward a still-cached v5 while a member is mid-receiving v6 (applied 4). Compared only to applied, v5 > 4 would supersede v6, then the arriving v6 chunks fail the v5 digest → stuck.
  - Why never bump here: a header-time bump makes the genuine blob complete as an equal-version no-op, leaving the receiver at `vN` with `v(N-1)` data.
- **Create the rcv-file** with `cryptoArgs = Nothing` (see Security), `file_type = roster`, `chat_item_id` NULL, `shared_msg_id` = the header's id. Accept it via `startRcvInlineFT` (chat-item-free), not `acceptRcvInlineFT`, so chunk 1 isn't rejected on `RFSNew`.
- **One in-flight per group is automatic**: the single `groups` row makes `roster_pending_*` single-valued, and there is one `(group_id, file_type = roster)` file. A duplicate header is idempotent. A version greater than both applied and pending supersedes: `UPDATE roster_pending_*` and delete the existing roster file (cleanup below), then create the new.

### Chunks

The header is enqueued before chunk 1 (per-connection FIFO).

**Reset-on-chunk-1** (decision 4): if chunk 1 arrives with partial chunks, discard and restart so relay restart / re-subscribe / QCONT can re-drive from the start. Discarding MUST (GAP 3):

- delete the `rcv_file_chunks` rows,
- truncate/remove the on-disk file, and
- evict its handle from the `rcvFiles` map (`closeFileHandle`).

`appendFileChunk` opens in AppendMode and caches the handle (`Internal.hs:1782-1788`), so clearing only the rows would append after the stale bytes and corrupt the blob (digest fails — the stuck state decision 4 avoids).

**Orphaned chunk**: a roster `BFileChunk` matching no in-flight `(group_id, shared_msg_id, file_type = roster)` file is **ACKed and ignored**, never errored (the version is already applied or superseded). This is how an up-to-date member tolerates the unconditional re-serve: the re-served header short-circuits (no file), then its chunks arrive with no transfer in flight. Distinct from reset-on-chunk-1, which fires only when partial chunks exist.

### Completion (on `RcvChunkFinal`)

1. Verify the assembled file's digest against `roster_pending_digest`. On mismatch, discard (delete the file, clear `roster_pending_*`); do not apply or bump.
2. **Version guard**: apply only if `roster_pending_version > roster_version` — a stale/out-of-order completion is rejected, not applied as a downgrade.
3. Parse → `validateGroupRoster` → `processRoster` (TOFU keys, role updates, revert absent promoted members, role-change items; pass `nameFromMemberId` where it used the entry name).

In **one transaction**: `processRoster` → set `roster_version = roster_pending_version` → set `roster_blob` → clear `roster_pending_*` → delete the file. A **relay** also promotes the pending signed-header columns into the live `roster_msg_*` and applies to its own records, then broadcasts (below). So a joiner never sees a live header at `vN` paired with a blob at `vN-1`.

The version guard plus the per-version `shared_msg_id` keying are what make the design correct; the short-circuit and one-in-flight-per-group are optimizations.

### Relay re-serve (broadcast / join / QCONT)

Per recipient, forward the signed header (as `forwardCachedRoster` does today) AND re-send the blob as `BFileChunk`s from `groups.roster_blob` (the send-from-bytes variant). An incoming `BFileChunk` returns no delivery task (`Subscriber.hs:1069`), so the blob send is driven here.

**No per-member version gate in v1 (GAP 2).** QCONT/SENT re-forwards the cached roster unconditionally today (`Subscriber.hs:1129`, `~1226`), and no per-member delivered-version tracker exists in the tree — this plan adds none. So a re-serve re-sends the whole blob on every drain; at cap 256 that is ~15 KB — one (occasionally two) `BFileChunk`s per drain — acceptable.

It is safe because: an up-to-date member short-circuits the header and ACK-ignores the orphaned chunks; a stale (≤ pending) re-forward mid-transfer is a no-op via the short-circuit; and the completion version guard rejects any stale completion.

If the cap is later raised so the blob spans many chunks, add a per-member `delivered_roster_version` column (read on QCONT/join/broadcast, written on confirmed delivery) and re-serve only when behind — future work.

### Supersede / cancel cleanup

Cleanup spans ALL of these — miss none:

- `files`, `rcv_files`, `rcv_file_chunks`,
- the on-disk file and its `rcvFiles` handle (`closeFileHandle`),
- the `roster_pending_*` columns on `groups` (set NULL).

## File-machinery changes (only these)

- **Lookup**: add `files.shared_msg_id`; resolve roster chunks by `(group_id, shared_msg_id, file_type = roster)`. Leave `getGroupFileIdBySharedMsgId` (`Store/Files.hs:310`, chat-item JOIN) for normal files; branch on `file_type` / `chat_item_id IS NULL`.
- **Fork the three receive sites that call `getChatItemByFileId`** (they throw with no chat item):
  - `startReceivingFile` (`Internal.hs:833`, reached on chunk 1) — skip the chat item + `CEvtRcvFileStart`.
  - `receiveFileChunk` `RcvChunkFinal` (`Subscriber.hs:1329`) — replace with the completion path above.
  - `FileChunkCancel` (`Subscriber.hs:1306`) — delete file + drop in-flight state, no chat item.
- **Cleanup keyed on `group_id`** (not chat items): `getGroupFileInfo` INNER-JOINs `chat_items`, so group delete (`Commands.hs:1270`) and clear (`1305`) skip roster files; the DB row cascades on group delete but the on-disk file leaks. Add a roster-file cleanup for delete/clear, cancel, and supersede.

## Storage / migration

In-flight state lives on `groups` (mirroring the live cache) and `files` (located by `shared_msg_id`) — no join table. New tail migration **after `M20260526_group_roster`** (SQLite + Postgres; register in `Migrations.hs` and `simplex-chat.cabal`; tests regenerate the schema files).

| `groups` column(s) | Holds | Lifecycle |
|---|---|---|
| `roster_version` *(kept)* | applied version | bumped at completion |
| `roster_msg_*` *(kept)* | live signed header (was full JSON) | relay forwards verbatim; promoted from pending at completion |
| `roster_blob` *(new)* | durable completed blob | written at completion; relay re-serves it |
| `roster_pending_version`, `roster_pending_digest` *(new)* | in-flight version + digest | set on header receipt; cleared at completion |
| `roster_pending_msg_*` *(new, relay-only)* | in-flight signed header | set on header receipt; promoted to live at completion (NULL on members) |

The kept `roster_msg_*` columns stay the relay's verbatim-forward source and trust anchor: `forwardCachedRoster` re-forwards them so the joiner verifies the owner signature, and the digest inside authenticates the unsigned blob.

`files` adds `shared_msg_id` and `file_type`. The in-flight transfer is the `files` / `rcv_files` / `rcv_file_chunks` rows with `(group_id, file_type = roster)`.

## Security

- **Owner-signed header**: assert `memberRole' author == GROwner` (`Subscriber.hs:3170`); keep `XGrpRoster_` in `requiresSignature`.
- **Integrity is entirely the digest** (S1): verify the assembled **plaintext** blob against the owner-signed `fileDigest` at completion. Hence `cryptoArgs = Nothing` — a set cryptoArgs makes `appendFileChunk` re-encrypt the file in place (`Internal.hs:1787`), so the on-disk bytes would be ciphertext and the check would fail. A corrupted chunk fails the digest and the roster is rejected.
- **TOFU** key pinning per `memberId` unchanged (different key for a known id → keep the trusted key).
- **Rollback (S2)**: the signature binds `publicGroupId + version` and the digest binds the blob to that header, so cross-group/version substitution stays blocked. But the blob now carries plain members, so a same-group replay of an old `(header, blob)` to a **new joiner** can re-introduce a removed poster or mask a demotion (existing members are protected by the version check). Update `channels-overview.md`.

## Known limitations / out of scope

- A malicious relay can withhold/corrupt chunks → the member stays on its last-applied roster (it can drop any message anyway); new-joiner rollback now covers plain members.
- A just-promoted member's first posts may show "unknown member" until the file arrives — self-healing.
- Out of scope: granting/enforcing `privileges`; member content signing; joiner-role-on-profile; clients. Do not couple the roster set to the joiner-role mechanism (decision 2) — it is the absolute `{member, mod, admin}`.

## Tests (`tests/ChatTests/Groups.hs`)

Update existing roster tests to header+file delivery, then add: digest-mismatch blob rejected (no apply, no version bump); member promotion enters the broadcast roster and can post; reset-on-chunk-1 recovery; superseding version cleans up the in-flight older file; version not bumped on header receipt or on a failed blob; `introduceInChannel` still mod+admin only (no member introductions); on-disk roster file cleaned on group delete/clear mid-transfer; non-owner promotion refused. Existing mod/admin tests must still pass.
