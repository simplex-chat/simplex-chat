# Roster: regular members + larger rosters via inline file

Date: 2026-06-01 (revised). Extends Section 1 of `2026-05-26-public-groups-via-relays-unified.md`. Anchors re-verified against the tree; the `xGrpRoster` block sits ~8 lines below the review-prompt numbers, and `encodeChatMessage` is in `Protocol.hs:909` (not `Internal.hs`). Confirm before editing.

## Goal

Let owners promote channel subscribers to **regular members** who can post, and carry more named members than fit one message. The JSON roster already exists (event, signing, relay cache, TOFU apply, broadcast, join forward, QCONT). This plan **changes only the delivery**: the member list moves out of the `XGrpRoster` message into a binary blob sent over the existing inline file transfer; `XGrpRoster` becomes a small signed header (version + the blob's size and digest). It also closes the review gaps below. The apply logic (`processRoster`) is reused.

## Roster set: widen to the promoted set {member, mod, admin}

Owners stay on the link, never in the roster. Two changes, then every gate follows:

- **Widen `isRosterRole`** (`Internal.hs:1230`) to `{GRMember, GRModerator, GRAdmin}`. All its call sites want the promoted set, so this one edit fixes: `validateGroupRoster` filter (`1236`, currently drops members — the bug), `buildGroupRoster` filter (`1248`), the promotion gates/cap/trigger/counts (`Commands.hs:2737,2739,2746,2762,2763,2768`), and the receive gates (`xGrpMemNew` `Subscriber.hs:2975/2990/3009`, `xGrpMemRole` owner-only `3157`, `xGrpLeave` refresh `3371`). Update the cap error text at `Commands.hs:2740`.
- **Split the query.** `getGroupRosterMembers` (`Store/Groups.hs:1214`) is used for two purposes that now diverge:
  - **Redefine** it to the promoted set (`member_role IN (GRMember, GRModerator, GRAdmin)`). Callers: `bumpAndBroadcastRoster` (`Internal.hs:2151`), `sendGroupRosterToRelay` (`2162`), `processRoster` revert set `currentPriv` (`Subscriber.hs:3207`). Build and revert use the **same** query — required, or a dropped member is never reverted.
  - **Add `getGroupModeratorMembers`** (mod+admin) and point **`introduceInChannel`** (`Internal.hs:1185`) at it. It must stay mod+admin: it announces the joiner to that set and introduces that set to the joiner, so widening it would introduce every member to every joiner (traffic + anonymity blowup). Members are learned from the roster blob instead.

Consequence to confirm: only the **owner** can change any roster role (member, mod, admin) — moderators cannot.

## Wire: header + binary blob

- **Header** `XGrpRoster { version :: VersionRoster, fileInv :: InlineFileInvitation }`, JSON, signed, forwarded. `InlineFileInvitation { fileSize :: Integer, fileDigest :: FD.FileDigest }` — a lean `FileInvitation` (no name/connReq/inline/descr; always inline). Tiny; fits `maxEncodedMsgLength`.
- **Blob** = the member list, binary. `RosterMember { memberId, key, role, privileges :: Word16 }` — drop `name`, add `privileges` (reserved: always `0`, parsed and ignored in v1, no storage/behaviour). ~53 B/entry. Roster-created members get a placeholder name from `nameFromMemberId`; real profiles arrive on first post.
- **Write a binary serializer + parser** (`smpEncode`/`smpP`) for the roster blob — a count-prefixed `[RosterMember]` — replacing the current JSON instance. This is the blob carried as `BFileChunk` payload: the owner serializes `[RosterMember]` → blob → digest → chunks; the receiver concatenates chunk bytes → verifies the digest → parses back to `[RosterMember]`.
- **Cap** `maxGroupRosterSize` → **256** (tunable). Enforce at promotion over the promoted set (the existing `Commands.hs:2739` check, via the widened predicate); re-validate the parsed entry count on receive (`Subscriber.hs:3171`); and reject a signed `fileSize > cap × max-entry-size` before creating any file. Roster files are **exempt** from the inline `offer/receiveChunks` ceiling (the cap is the only bound; at 256 ≈ 13.5 KB the blob is one chunk anyway).

## Delivery over the inline file transfer

**Owner send** (`bumpAndBroadcastRoster`, `sendGroupRosterToRelay`): build the blob with `buildGroupRoster` over the widened query, compute its `FileDigest`, send `XGrpRoster` (header), then send the blob as `BFileChunk`s against that message's `shared_msg_id`. `sendFileInline_` reads from a file, so add a send-from-bytes variant (shared with the relay re-serve). The owner's own version bump stays as today (`Internal.hs:2148/2152`) — the owner is the source of truth; the "bump only at completion" rule below is for the receive side.

**Header handler** (`xGrpRoster`, member and relay): this no longer applies anything — it starts a transfer.
- Short-circuit if `version ≤` applied `rosterVersion` **before** creating a file. Never call `setGroupRosterVersion`/`setCachedGroupRoster` here (else the genuine blob is later rejected as an equal-version no-op and the receiver is stuck at `vN` with `v(N-1)` data).
- Create the group rcv-file with `cryptoArgs = Nothing` (see Security), `file_type = roster`, `chat_item_id` NULL, `shared_msg_id` = the header's id; accept it via `startRcvInlineFT` (chat-item-free), not `acceptRcvInlineFT`, so chunk 1 isn't rejected on `RFSNew`.
- One in-flight roster file per group (key by `group_id`). A duplicate header (same `shared_msg_id`) is idempotent; a newer version supersedes — delete the stale file + chunks, create the new.

**Chunks**: header is enqueued before chunk 1 (per-connection FIFO). **Reset-on-chunk-1** (decision 4): if chunk 1 arrives with partial chunks, discard them and restart, so relay restart / re-subscribe / QCONT can always re-drive from the start.

**Completion** (on `RcvChunkFinal`): verify the assembled file's digest against the signed `fileDigest`; on mismatch, discard and do not apply or bump. Then parse the blob → `validateGroupRoster` → `processRoster` (TOFU keys, role updates, revert absent promoted members, role-change items; pass `nameFromMemberId` where it used the entry name). **In one transaction**: `processRoster` + `setGroupRosterVersion`. Then delete the file. **Relay also**, in the same transaction: cache the signed header (`setCachedGroupRoster`) + write the blob to `groups.roster_blob` + apply to its own records; then broadcast (below).

**Relay re-serve** (broadcast / join / QCONT): per recipient, forward the signed header (as `forwardCachedRoster` does today) **and** re-send the blob as `BFileChunk`s from `groups.roster_blob` (the send-from-bytes variant), gated on a per-member delivered version. An incoming `BFileChunk` returns no delivery task (`Subscriber.hs:1069`), so the blob send must be driven explicitly here.

## File-machinery changes (only these)

- **Lookup**: add `files.shared_msg_id`; resolve roster chunks by `(group_id, shared_msg_id, file_type=roster)`. Leave `getGroupFileIdBySharedMsgId` (`Store/Files.hs:310`, chat-item JOIN) for normal files; branch on `file_type`/`chat_item_id IS NULL`.
- **Fork the three completion/receive sites that call `getChatItemByFileId`** (they throw with no chat item): `startReceivingFile` (`Internal.hs:833`, reached on chunk 1) — skip the chat-item + `CEvtRcvFileStart`; `receiveFileChunk` `RcvChunkFinal` (`Subscriber.hs:1329`) — replace with the completion path above; `FileChunkCancel` (`1306`) — delete file + drop in-flight state, no chat item.
- **Cleanup keyed on `group_id`** (not chat items): `getGroupFileInfo` INNER-JOINs `chat_items`, so group delete (`Commands.hs:1270`) and clear (`1305`) skip roster files; the DB row cascades on group delete but the on-disk file leaks. Add a roster-file cleanup for group delete/clear, cancel, and supersede. (Steady state has no on-disk file — it's deleted at completion; the durable copy is `groups.roster_blob`.)

## Storage / migration

New tail migration **after `M20260526_group_roster`** (SQLite + Postgres; register in `Migrations.hs` and `simplex-chat.cabal`; tests regenerate `chat_schema.sql`/`chat_query_plans.txt`):
- `groups.roster_blob` — durable completed blob (relay re-serves from it).
- `files.shared_msg_id`, `files.file_type`.
- `group_roster_recv(group_id PK, shared_msg_id, version, file_digest, file_id, + signed-header columns for relays)` — in-flight state; PK enforces one-per-group and makes supersede an upsert; cleared at completion.

## Security

- Header must be **owner-signed** (`memberRole' author == GROwner`, `Subscriber.hs:3170`); keep in `requiresSignature`.
- **Integrity is entirely the digest** (S1): verify the assembled **plaintext** blob against the owner-signed `fileDigest` at completion. Hence `cryptoArgs = Nothing` — a set cryptoArgs would make `appendFileChunk` re-encrypt the file in place (`Internal.hs:1787`), so the on-disk bytes would be ciphertext and the check would fail. Chunks are otherwise unsigned; a corrupted chunk fails the digest and the roster is rejected.
- TOFU key pinning per `memberId` unchanged (different key for a known id → keep trusted key).
- Anti-replay (S2): signature binds `publicGroupId + version`, digest binds the blob to that header, so cross-group/version substitution stays blocked. But the blob now carries plain members, so a same-group replay of an old `(header, blob)` to a **new joiner** can re-introduce a removed poster or mask a demotion (existing members are still protected by the version check). Update `channels-overview.md`.

## Known limitations / out of scope

- A malicious relay can withhold/corrupt chunks → the member stays on its last-applied roster (it can drop any message anyway); new-joiner rollback now covers plain members (above).
- A just-promoted member's first posts may show "unknown member" until the file arrives — self-healing.
- Out of scope: granting/enforcing `privileges`; member content signing; joiner-role-on-profile; clients. Do not couple the roster set to the joiner-role mechanism (decision 2) — it's the absolute `{member, mod, admin}`.

## Tests (`tests/ChatTests/Groups.hs`)

Update existing roster tests to header+file delivery, then add: digest-mismatch blob rejected (no apply, no version bump); member promotion enters the broadcast roster and can post; reset-on-chunk-1 recovery; superseding version cleans up the in-flight older file; version not bumped on header receipt or on a failed blob; `introduceInChannel` still mod+admin only (no member introductions); on-disk roster file cleaned on group delete/clear mid-transfer; non-owner promotion refused. Existing mod/admin tests must still pass.
