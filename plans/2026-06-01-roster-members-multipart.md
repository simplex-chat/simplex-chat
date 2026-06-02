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

Consequence to confirm: only the **owner** can change any roster role (member, mod, admin) — moderators cannot. The widened predicate also makes every promoted-member **leave** trigger `bumpAndBroadcastRoster` (`Subscriber.hs:3379`; the remove path `Commands.hs:2888`) — a member leaving rebroadcasts the roster. Acceptable for channels (owner-paced, low frequency), but called out.

## Wire: header + binary blob

- **Header** `XGrpRoster { version :: VersionRoster, fileInv :: InlineFileInvitation }`, JSON, signed, forwarded. `InlineFileInvitation { fileSize :: Integer, fileDigest :: FD.FileDigest }` — a lean `FileInvitation` (no name/connReq/inline/descr; always inline). Tiny; fits `maxEncodedMsgLength`.
- **Blob** = the member list, binary. `RosterMember { memberId, key, role, privileges :: Word16 }` — drop `name`, add `privileges` (reserved: always `0`, parsed and ignored in v1, no storage/behaviour). ~53 B/entry. Roster-created members get a placeholder name from `nameFromMemberId`; real profiles arrive on first post.
- **Write a binary serializer + parser** (`smpEncode`/`smpP`) for the roster blob — a count-prefixed `[RosterMember]` — replacing the current JSON instance. This is the blob carried as `BFileChunk` payload: the owner serializes `[RosterMember]` → blob → digest → chunks; the receiver concatenates chunk bytes → verifies the digest → parses back to `[RosterMember]`.
- **Cap** `maxGroupRosterSize` → **256** (tunable). Enforce at promotion over the promoted set (the existing `Commands.hs:2739` check, via the widened predicate); re-validate the parsed entry count on receive (`Subscriber.hs:3171`); and reject a signed `fileSize > cap × max-entry-size` before creating any file. Roster files are **exempt** from the inline `offer/receiveChunks` ceiling (the cap is the only bound; at 256 ≈ 13.5 KB the blob is one chunk anyway).

## Delivery over the inline file transfer

**Owner send** (`bumpAndBroadcastRoster`, `sendGroupRosterToRelay`): build the blob with `buildGroupRoster` over the widened query, compute its `FileDigest`, send `XGrpRoster` (header), then send the blob as `BFileChunk`s against that message's `shared_msg_id`. `sendFileInline_` reads from a file, so add a send-from-bytes variant (shared with the relay re-serve). The owner's own version bump stays as today (`Internal.hs:2148/2152`) — the owner is the source of truth; the "bump only at completion" rule below is for the receive side.

**Header handler** (`xGrpRoster`, member and relay): this no longer applies anything — it starts a transfer.
- Short-circuit on receipt (both `memberApplyRoster` and `relayApplyRoster`) unless `version > max(groups.roster_version, groups.roster_pending_version)` — strictly greater than BOTH the applied and any pending version, **before** creating a file. Comparing only against the applied version is a bug (GAP 1): the QCONT re-serve below is unconditional, so the relay can re-forward its still-cached v5 while the member is mid-receiving v6 (applied = 4); v5 > 4 would supersede the in-flight v6, and the arriving v6 chunks then fail the v5 digest → member stuck. The header handler only sets `roster_pending_*` and starts the transfer — never write `roster_version` or the live `roster_msg_*` here (else the genuine blob completes as an equal-version no-op and the receiver is stuck at `vN` with `v(N-1)` data).
- Create the group rcv-file with `cryptoArgs = Nothing` (see Security), `file_type = roster`, `chat_item_id` NULL, `shared_msg_id` = the header's id; accept it via `startRcvInlineFT` (chat-item-free), not `acceptRcvInlineFT`, so chunk 1 isn't rejected on `RFSNew`.
- One in-flight roster file per group is automatic: the single `groups` row makes `roster_pending_*` single-valued, and there is one `(group_id, file_type = roster)` file. A duplicate header (same `shared_msg_id`/version) is idempotent. A version greater than both applied and pending supersedes — `UPDATE roster_pending_*` and delete the existing roster file (cleanup below), then create the new.

**Chunks**: header is enqueued before chunk 1 (per-connection FIFO). **Reset-on-chunk-1** (decision 4): if chunk 1 arrives with partial chunks, discard the partial transfer and restart. Discarding MUST (GAP 3) delete the `rcv_file_chunks` rows **and truncate/remove the on-disk file and evict its handle from the `rcvFiles` handle map** (`closeFileHandle`) — `appendFileChunk` opens in AppendMode and caches the handle (`Internal.hs:1782-1788`), so clearing only the chunk rows would append the restarted bytes after the stale ones and corrupt the blob (digest fails — the exact stuck state decision 4 avoids). This lets relay restart / re-subscribe / QCONT re-drive from the start.

**Orphaned roster chunk**: a roster `BFileChunk` whose `(group_id, shared_msg_id, file_type = roster)` matches no in-flight roster file is **ACKed and ignored** (the version is already applied or superseded) — it must NOT error. This is how an up-to-date member tolerates the unconditional QCONT re-serve: the re-served header is short-circuited (no file created), then its chunk(s) arrive with no transfer in flight. Distinct from reset-on-chunk-1, which fires only when partial chunks already exist.

**Supersede / cancel cleanup** spans ALL of: `files`, `rcv_files`, `rcv_file_chunks`, the on-disk file, the `rcvFiles` handle map (`closeFileHandle`), and the `roster_pending_*` columns on `groups` (set NULL) — miss none.

**Completion** (on `RcvChunkFinal`): verify the assembled file's digest against `roster_pending_digest`; on mismatch, discard (delete the file, clear `roster_pending_*`) and do not apply or bump. **Completion-time version guard:** apply only if `roster_pending_version > roster_version` — a stale or out-of-order completion is rejected, not applied as a downgrade. This guard, together with the per-version `shared_msg_id` keying of chunks, is what makes the design correct; the header short-circuit and one-in-flight-per-group are then optimizations, not correctness requirements. Parse the blob → `validateGroupRoster` → `processRoster` (TOFU keys, role updates, revert absent promoted members, role-change items; pass `nameFromMemberId` where it used the entry name). **In one transaction:** `processRoster` → set `roster_version = roster_pending_version` → set `roster_blob` → (relay) promote the pending signed-header columns into the live `roster_msg_*` → clear `roster_pending_*` → delete the file. So a joiner never sees a live header at `vN` paired with a blob at `vN-1`; then broadcast (below).

**Relay re-serve** (broadcast / join / QCONT): per recipient, forward the signed header (as `forwardCachedRoster` does today) **and** re-send the blob as `BFileChunk`s from `groups.roster_blob` (the send-from-bytes variant). An incoming `BFileChunk` returns no delivery task (`Subscriber.hs:1069`), so the blob send is driven here. **No per-member version gate in v1 (GAP 2):** QCONT/SENT re-forwards the cached roster **unconditionally** today (`Subscriber.hs:1129` and `~1226`), and there is no per-member delivered-roster-version tracker in the tree — this plan does not add one, so a re-serve re-sends the whole blob on every drain. At cap 256 the blob is a single `BFileChunk` (~13.5 KB), so that's one extra message per drain — acceptable. An up-to-date member short-circuits the re-served header and ACK-ignores its orphaned chunk(s) (above); a stale (≤ pending) re-forward mid-transfer is a no-op via the `max(roster_version, roster_pending_version)` short-circuit; and the completion version guard rejects any stale completion regardless. If the cap is later raised so the blob spans many chunks, add a per-member `delivered_roster_version` column (read on QCONT/join/broadcast, written on confirmed delivery) and re-serve only when the member is behind — future work, not v1.

## File-machinery changes (only these)

- **Lookup**: add `files.shared_msg_id`; resolve roster chunks by `(group_id, shared_msg_id, file_type=roster)`. Leave `getGroupFileIdBySharedMsgId` (`Store/Files.hs:310`, chat-item JOIN) for normal files; branch on `file_type`/`chat_item_id IS NULL`.
- **Fork the three completion/receive sites that call `getChatItemByFileId`** (they throw with no chat item): `startReceivingFile` (`Internal.hs:833`, reached on chunk 1) — skip the chat-item + `CEvtRcvFileStart`; `receiveFileChunk` `RcvChunkFinal` (`Subscriber.hs:1329`) — replace with the completion path above; `FileChunkCancel` (`1306`) — delete file + drop in-flight state, no chat item.
- **Cleanup keyed on `group_id`** (not chat items): `getGroupFileInfo` INNER-JOINs `chat_items`, so group delete (`Commands.hs:1270`) and clear (`1305`) skip roster files; the DB row cascades on group delete but the on-disk file leaks. Add a roster-file cleanup for group delete/clear, cancel, and supersede. (Steady state has no on-disk file — it's deleted at completion; the durable copy is `groups.roster_blob`.)

## Storage / migration

In-flight roster state lives on `groups` (mirroring the live cache) and `files` (located by `shared_msg_id`) — no join table. New tail migration **after `M20260526_group_roster`** (SQLite + Postgres; register in `Migrations.hs` and `simplex-chat.cabal`; tests regenerate `chat_schema.sql`/`chat_query_plans.txt`):

- **Keep the existing live columns** (`roster_version`; `roster_msg_body` / `roster_msg_chat_binding` / `roster_msg_signatures` / `roster_sending_owner_gm_id` / `roster_broker_ts`) — NOT removed. They stay the relay's verbatim-forward source and trust anchor: `forwardCachedRoster` re-forwards them to joiners so the joiner verifies the owner signature, and the digest inside authenticates the unsigned blob. Their contents just shrink — from the full roster JSON to the small signed header (version + `InlineFileInvitation{size, digest}`).
- **Add `groups.roster_blob`** — the durable completed blob; the relay re-serves it as `BFileChunk`s.
- **Add the pending set on `groups`** (NULL in steady state; written on header receipt, promoted into the live columns and cleared at completion):
  - `roster_pending_version`, `roster_pending_digest` — used by member and relay to compare versions (GAP 1) and verify the assembled blob at completion.
  - relay-only `roster_pending_msg_body` / `_chat_binding` / `_signatures` / `_owner_gm_id` / `_broker_ts` — the in-flight signed header, promoted into the live `roster_msg_*` at completion. Members leave these NULL (they don't re-serve).
- **Add `files.shared_msg_id`, `files.file_type`.** The in-flight transfer is the `files`/`rcv_files`/`rcv_file_chunks` rows with `(group_id, file_type = roster)`; chunks match by `(group_id, shared_msg_id, file_type = roster)`. At most one in-flight roster file per group (one `groups` row ⇒ single-valued pending columns).

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
