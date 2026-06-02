# Roster: regular members + larger rosters via inline file

Date: 2026-06-01 (revised). Extends Section 1 of `2026-05-26-public-groups-via-relays-unified.md`.

Anchors here were re-verified against the tree on revision; see the **Anchors** section at the end for what drifted. Confirm before editing — the `xGrpRoster` handler block in particular sits ~8 lines below the numbers in the source review prompt.

## Problem

Channels pin every subscriber to observer, so they can't post. We want owners to promote some subscribers to **regular members** who can post, and to support more named members than fit in one message. Today the roster (`XGrpRoster`) is an owner-signed, single-message JSON snapshot of moderators + admins only, capped at 64 (`maxGroupRosterSize`, `Protocol.hs:897`).

## What already exists (this is a delivery-mechanism change, not a greenfield build)

The JSON roster is fully implemented: the `XGrpRoster` event, owner-signature signing/verification, the relay cache on `groups` (`roster_msg_*`), TOFU apply, broadcast-on-change, join-time forward, and the QCONT catch-up. This plan changes **how the member list is delivered** — from one signed JSON message to a signed JSON header plus a binary blob carried over the existing inline file transfer — and closes the review gaps. Reuse the apply *logic* (`processRoster`); replace the *delivery, parsing, and version-bump timing*.

## The change in one sentence

The roster becomes the owner-signed list of **all promoted members** — member, moderator, admin (owners stay on the link, never in the roster) — delivered by a small signed `XGrpRoster` **header** carrying the blob's size and digest, with the packed binary member list sent as `BFileChunk` over the existing inline file machinery.

## Why a roster, and why members belong in it

The relay defaults every joiner to observer, and it forwards a member's posts only if its own record says that member's role is above observer (`memberCanSend`, `Subscriber.hs:1617`). Role is owner-asserted only — a relay must never invent a poster. So a member who can post must be established in the owner-signed roster, exactly like a moderator. The roster is how the relay authorizes posting and how members learn the keys and roles of the promoted set. The owner already learns each subscriber's key when they join (the relay announces joiners to owners via `XGrpMemNew`, `introduceInChannel`), so promoting one needs no new key exchange.

## Roster entry and wire format (F1, F2)

The `XGrpRoster` **header** payload changes from carrying the member list inline to carrying a lean inline-file invitation:

- `XGrpRoster { version :: VersionRoster, fileInv :: InlineFileInvitation }`, JSON, signed, forwarded. Drop the inline `roster :: [RosterMember]`.
- `InlineFileInvitation { fileSize :: Integer, fileDigest :: FD.FileDigest }` — a lean `FileInvitation` with no `fileName` / `fileConnReq` / `fileInline` / `fileDescr` (it is always an inline send). Reuse `FD.FileDigest`.
- The header is tiny (version + size + a digest); it fits `maxEncodedMsgLength` (15602) trivially.

The **blob** is the member list, binary-packed: `smpEncode` of `[RosterMember]`, with `RosterMember { memberId, key, role, privileges :: Word16 }` — **drop `name`**, add `privileges`. ~53 B/entry. A roster-created member gets a placeholder name from its id (`nameFromMemberId`); its real profile arrives when it posts. `privileges` is reserved: always packed as `0`, parsed and ignored in v1 — no storage, no behaviour, no enforcement (the first intended bit, "post as the channel", is out of scope).

Cap: raise `maxGroupRosterSize` to **256 (tunable)**. Enforce at promotion over the **promoted set** (the existing check at `Commands.hs:2739`, which follows `isRosterRole` once widened — see C1). On receive, re-validate the parsed entry count `≤ maxGroupRosterSize` (the existing guard `length entries > maxGroupRosterSize`, now `Subscriber.hs:3171`, runs on the parsed blob), and reject an owner-signed `fileSize > maxGroupRosterSize × max-entry-size` **before** creating any rcv-file (defense). Per decision 3, roster files are **exempt** from the inline ceiling (`offerChunks`/`receiveChunks`) — the cap is the only size bound; at cap 256 (~13.5 KB) the blob is under one `fileChunkSize` (15780) chunk, so the exemption only matters if the cap is later raised.

## The promoted-set vs moderation-set split (C1)

`isRosterRole` and `getGroupRosterMembers` are overloaded across diverging responsibilities. Split them:

- **Widen the predicate.** `isRosterRole` (`Internal.hs:1230`) becomes `r ∈ {GRMember, GRModerator, GRAdmin}`. Every call site wants the promoted set, so this one change cascades correctly:
  - `validateGroupRoster` filter (`Internal.hs:1236`) — **fixes the bug** where member entries are currently dropped.
  - `buildGroupRoster` filter (`Internal.hs:1248`) — members are now packed into the blob.
  - Promotion path (`Commands.hs`): owner-only gate (`2737`), cap (`2739`), `rosterSetChanged` trigger (`2746`), and the `anyPrivTarget`/`privCount` accumulators (`2762`, `2763`, `2768`) — member promotions are now owner-gated, counted toward the cap, and **trigger the broadcast** (without this the feature silently fails: a promoted member never enters the broadcast roster and relays never authorize their posts). Update the cap error text at `2740` (currently "moderators and admins").
  - Receive gates: `xGrpMemNew` privileged-role checks (`Subscriber.hs:2975/2990/3009`), `xGrpMemRole` owner-only gate (`Subscriber.hs:3157`), `xGrpLeave` roster-refresh trigger (`Subscriber.hs:3371`) — a relay-asserted member must now be roster-established (relay can't conjure a poster), only the owner changes a member's role, and a promoted member leaving refreshes the roster.
- **Split the query.** `getGroupRosterMembers` (`Store/Groups.hs:1214`, currently `member_role IN (mod, admin)`) is used for two different purposes:
  - **Redefine** `getGroupRosterMembers` to the **promoted set** `member_role IN (GRMember, GRModerator, GRAdmin)`. Callers: `bumpAndBroadcastRoster` (`Internal.hs:2151`, build), `sendGroupRosterToRelay` (`Internal.hs:2162`, build), `processRoster` revert set `currentPriv` (`Subscriber.hs:3207`). The build set and the revert set are now the **same** query — required, or a dropped member is never reverted.
  - **Add** `getGroupModeratorMembers` (`member_role IN (GRModerator, GRAdmin)`) and repoint **`introduceInChannel`** (`Internal.hs:1185`) to it. This MUST stay mod+admin: it both announces the joiner to that set and introduces that set to the joiner, so widening it would announce every subscriber to every member and introduce every member to every joiner (traffic + subscriber-anonymity blowup).

## Owner build and send

`bumpAndBroadcastRoster` (bump version, broadcast to relays) and `sendGroupRosterToRelay` (current roster to a newly added relay, no bump) build the blob with `buildGroupRoster` over the widened `getGroupRosterMembers` (promoted set), `smpEncode` it, compute its `FileDigest`, then send `XGrpRoster { version, InlineFileInvitation { fileSize, fileDigest } }` to the relay(s), followed by the blob as `BFileChunk`s against that message's `shared_msg_id`. The owner holds the blob in memory (from `buildGroupRoster`), so it has the same need as the relay re-serve (E2): `sendFileInline_` reads from a file on disk, so either materialize the blob to a temp file or add a send-from-bytes inline-send variant (shared with E2). The owner's own `roster_version` bump stays where it is today (`bumpAndBroadcastRoster`, `Internal.hs:2148/2152`) — the owner is the source of truth; B1's "bump only at completion" rule is about the **receive** side (relay and member).

## Delivery state machine: header → chunks → completion (B1–B4)

The single-message handler currently does check-version + `processRoster` + `setCachedGroupRoster`/`setGroupRosterVersion` **atomically on one message** (`Subscriber.hs:3179-3201`), because header and data are the same message. The split decomposes this: the header starts a transfer; **apply + version bump happen only at blob completion**.

### Header handler (`xGrpRoster`, both member and relay)

- **Short-circuit on version BEFORE any side effect (B2):** if `version ≤` the applied `rosterVersion gInfo`, drop — do not create an rcv-file. (Mirrors today's `newVer <=` relay check and `newVer <`/`==` member checks, but now gating *file creation*, not apply.) Reject `fileSize` over the cap bound here too.
- **Never touch `roster_version` (B1).** The header handler must not call `setGroupRosterVersion` or `setCachedGroupRoster`. If it did, the genuine blob would later be rejected as an equal-version no-op and the member/relay would be stuck at `vN` holding `v(N-1)` data.
- **Create/accept the rcv-file chat-item-free (B4):** create the group rcv-file from the `InlineFileInvitation` with **`cryptoArgs = Nothing`** (no `file_crypto_key`/`nonce`), `chat_item_id` NULL, the new `file_type = roster`, and `shared_msg_id` = the `XGrpRoster` message id. Move it to `RFSAccepted` via **`startRcvInlineFT`** (`Store/Files.hs:578`), NOT `acceptRcvInlineFT` (`576`, which calls `getChatItemByFileId`). Without `RFSAccepted`, chunk 1 is rejected by `receiveInlineChunk` (`Subscriber.hs:2441`, `CEInlineFileProhibited` on `RFSNew`).
- **In-flight keying / idempotency (B2):** at most one in-flight roster file per group. Key it by `group_id` (+ `file_type = roster`). A duplicate header (same `shared_msg_id`) is idempotent — reuse the existing file, don't create a second. A **newer** version (new `shared_msg_id`) **supersedes** the in-flight older one: cancel/delete the stale rcv-file and its chunks, then create the new (B3).
- **Persist what completion needs.** Hold, for the in-flight transfer: `version`, the expected `fileDigest`, the `shared_msg_id`, the `file_id`, and (relay only) the **signed header bytes** for later re-forward. Recommended: one row per group in a new `group_roster_recv` table (PK `group_id`), so the PK enforces "one in-flight per group" and supersede is an upsert. The live completed cache (below) is untouched until completion, so joiners are still served the last completed roster during an in-flight transfer.

### Chunks (B4 ordering)

The member receives the header via `XGrpMsgForward`/`forwardCachedRoster` but the chunks via direct `BFileChunk` (dispatch `Subscriber.hs:1069` → `bFileChunkGroup` `Subscriber.hs:2435`). Required: the header is enqueued **before** chunk 1, relying on per-connection FIFO so the rcv-file exists and is `RFSAccepted` when chunk 1 arrives. `bFileChunkGroup` must resolve roster chunks via the new `files.shared_msg_id` lookup (D2), not the chat-item JOIN.

**Reset-on-chunk-1 (decision 4, B3):** `createRcvFileChunk` (`Store/Files.hs:656`) returns `RcvChunkError` when chunk 1 arrives with existing chunks. For roster files, the receive path must instead **discard the partial state** (delete `rcv_file_chunks`, truncate the on-disk file) and restart from chunk 1, so relay restart, re-subscribe, and QCONT catch-up can always re-drive from the start. Recovery: an interrupted transfer resumes by the sender re-driving from chunk 1 (the relay re-drives its inline send; the agent redelivers in-flight chunks). The completion detector (`chunkSize >= fileSize` ⇒ `RcvChunkFinal`, else cumulative) is reused as-is once chunk numbering restarts cleanly.

### Completion (member)

On `RcvChunkFinal`, the roster fork (D1) replaces the chat-item path:

1. `appendFileChunk` writes the final chunk. Because `cryptoArgs = Nothing`, the on-disk file is **plaintext** (the in-place encryption at `Internal.hs:1787-1794` does NOT run — this is the S1 invariant).
2. Read the assembled file, compute its digest, and **verify against the owner-signed `fileDigest`**. On mismatch: discard, do not apply, do not bump version (relay corruption is caught here).
3. Parse the blob → `[RosterMember]` → `validateGroupRoster` → `processRoster` (existing logic: TOFU key pinning, role updates, revert of promoted members absent from the new roster via the widened `getGroupRosterMembers`, role-change chat items via `emitRosterResults`). Pass `nameFromMemberId memberId` where `processRoster` currently uses the entry `name`.
4. **In one transaction:** `processRoster` + `setGroupRosterVersion (version)`. Then delete the on-disk file and the `group_roster_recv` row.

### Completion (relay)

Same as the member, plus: **in the same transaction**, promote the held signed header bytes into the live cache (`setCachedGroupRoster`, which writes `roster_msg_*`), write the assembled blob to the new `groups.roster_blob` column, set `roster_version`, and apply to its own member records. Then trigger the broadcast (E2). This is the only place `roster_version` and the live cache move forward on a relay.

## File lifecycle: the NULL `chat_item_id` cross-cut (D1, D2, D3)

**D1 — sites that call `getChatItemByFileId` and throw for a roster file, with the fork:**
- `startReceivingFile` (`Internal.hs:833`), reached from `receiveInlineChunk` on chunk 1 (`Subscriber.hs:2446`): roster files must take a variant that skips the chat-item lookup and the `CEvtRcvFileStart` view event.
- `receiveFileChunk` `RcvChunkFinal` (`Subscriber.hs:1329`): replaced by the completion fork above (verify-digest → save blob → delete file → apply).
- `receiveFileChunk` `FileChunkCancel` (`Subscriber.hs:1306`): a roster transfer cancel routes to delete-file + drop the `group_roster_recv` row, no chat item.
- `acceptRcvInlineFT` (`Store/Files.hs:576`) is avoided entirely (we use `startRcvInlineFT`).

**D2 — lookup:** add `files.shared_msg_id` and a roster-file lookup keyed on `(group_id, shared_msg_id, file_type = roster)`, used by `bFileChunkGroup` for roster chunks. Do **not** change `getGroupFileIdBySharedMsgId` (`Store/Files.hs:310`, the chat-item JOIN) for normal files — add a roster-specific resolver and branch on `file_type` / `chat_item_id IS NULL`.

**D3 — cleanup keyed on `group_id`, not chat items:** group delete (`deleteChat` `Commands.hs:1270`) and clear (`APIClearChat` `Commands.hs:1305`) collect files via `getGroupFileInfo`, which INNER-JOINs `chat_items` (`Store/Shared.hs` `fileInfoQuery`) and therefore **skips roster files** (NULL `chat_item_id`). The `files` row cascades on group delete via `group_id`, but the **on-disk file does not** — it leaks. Add a dedicated roster-file cleanup, keyed on `group_id`, that removes the on-disk file (and the `group_roster_recv` row). Cover: group delete, group clear, transfer cancel, and superseded/stale cleanup. (At steady state there is no on-disk roster file — it's deleted at completion and the durable copy is `groups.roster_blob`; the on-disk file exists only during an in-flight transfer, so the leak window is an in-flight transfer at delete/clear time.)

## Relay cache, blob, and re-serve (E1, E2, E3)

**E1 — cache both, atomically at completion:** the relay caches the **signed header bytes** (`roster_msg_*`, forwarded verbatim) AND the **blob** (new durable column `groups.roster_blob`), and bumps `roster_version` + applies to its own member records, all in one transaction at blob completion (see "Completion (relay)"). New tail migration **after `M20260526_group_roster`** adds `groups.roster_blob`, `files.shared_msg_id`, `files.file_type`, and the `group_roster_recv` table; register in both SQLite and Postgres `Migrations.hs` and `simplex-chat.cabal`; let tests regenerate `chat_schema.sql`/`chat_query_plans.txt`. Never edit an applied migration.

**E2 — re-serve the blob, not just the header:** today `relayApplyRoster` returns `DJSGroup` (`Subscriber.hs:3189`) and the broadcast re-forwards only the header via `forwardCachedRoster`. An incoming `BFileChunk` returns no delivery task (`Subscriber.hs:1069`), so the blob is not auto-forwarded. The relay's broadcast and join/QCONT re-serve must, per recipient: (1) forward the signed `XGrpRoster` header (as `forwardCachedRoster` does), then (2) **re-drive an inline send of the blob** from `groups.roster_blob` as `BFileChunk`s against the same `shared_msg_id`. `sendFileInline_` (`Internal.hs:1648`) reads from a file on disk, so either materialize the cached blob to a temp file or add a send-from-bytes variant. Gate the re-send on a per-member **delivered roster version** so members already at the current version are not re-served (unified §1.6 QCONT catch-up). Given decision 4, a re-drive always restarts from chunk 1, so an interrupted re-serve is safe to repeat.

**E3 — amplification:** a broadcast costs `N_members × N_chunks`. At cap 256 that is `N_members × 1`. Above the cap it grows; v1 keeps the single uniform file path. If the per-change cost of routing even a small mod-only roster through the file transfer matters, a size threshold (inline-when-small) is a later optimization — at the cost of a second code path. v1 does not take it.

## Trust and safety (S1, S2)

- The header must be **signed by an owner** (assert `memberRole' author == GROwner`, `Subscriber.hs:3170`) — a relay can't forge it or re-attribute it to a member whose key it controls. Keep `XGrpRoster_` in `requiresSignature`.
- **Integrity rests entirely on the digest (S1).** The load-bearing invariant: the assembled blob is verified against the owner-signed `fileDigest` at completion, and the digest is computed over the **raw plaintext blob**. The roster rcv-file MUST have `cryptoArgs = Nothing` so `appendFileChunk`'s in-place encryption (`Internal.hs:1787-1794`) never runs and the on-disk bytes equal the digested bytes. Hash after the final chunk is written and the file handle is closed, before any transform — there is no transform for roster files.
- Chunks are unsigned; their only integrity is that signed digest. A relay that corrupts a chunk fails the digest and the roster is rejected (the member stays on its last-applied roster).
- A member's key is **trust-on-first-use**, pinned per id — a different key for a known id is rejected, never overwritten (`processRoster` conflict path).
- **Anti-replay / rollback (S2):** the signature binds `CBGroup <> (publicGroupId, ownerMemberId)` (`Subscriber.hs:3573`) and the digest binds the blob to that signed header, so cross-group and cross-version substitution stay blocked. But the blob now carries **plain members**, so a same-group replay of an old `(header, blob)` pair to a **new joiner** can re-introduce a removed poster or mask a demotion (existing members are still protected by the version check). Update `channels-overview.md`: the rollback surface now includes member authorization, not just moderator keys.

## Consequence to confirm

Because the roster is owner-signed, **only the owner can promote or demote any roster role** — member, moderator, or admin (moderators cannot). This is decision 1, enforced by the widened `isRosterRole` at the gates above. Consistent with the trust model; flagged for confirmation.

## Known limitations (inherited, not introduced here)

- A malicious relay can withhold or corrupt chunks, leaving a member on its last-applied roster — the same staleness a relay can impose by dropping any message. Existing members are still protected from rollback by the version check; the new-joiner rollback gap (now covering plain members, S2) is documented in `channels-overview.md`.
- A just-promoted member's first posts may render as "unknown member" until the roster file finishes arriving — self-healing on the next post.

## Out of scope

Granting/enforcing `privileges`; signing member message content; the joiner-role-on-profile change; iOS/Kotlin clients (including hiding unknown rows). Do NOT couple the promoted set to the joiner-role / public-group mechanism from the follow-up plan (decision 2): the roster set is the absolute `{member, mod, admin}`.

## Tests (Haskell, `tests/ChatTests/Groups.hs`)

Update the existing roster tests to the header+file delivery, then add:

- **Digest-mismatch blob rejected** (simulate relay corruption of a chunk): roster not applied, no roles changed, version not bumped.
- **Member promotion enters the broadcast roster and is authorized to post**: owner promotes a subscriber to `GRMember`; the member posts; the relay forwards (its `memberCanSend` now allows); other members receive it.
- **Reset-on-chunk-1 recovery**: chunk 1 redelivered after a partial receipt restarts cleanly and completes.
- **Superseding version cleans up the in-flight older transfer** (new `shared_msg_id` ⇒ new file; stale file + chunks removed).
- **Version NOT bumped on header receipt** and **not bumped on failed (digest-mismatch) blob** — assert the receiver is not left at `vN` with `v(N-1)` data.
- **`introduceInChannel` still mod+admin only** — joining a channel with promoted members introduces no members to the joiner (the joiner learns them from the roster blob).
- **On-disk roster file cleaned on group delete/clear** while a transfer is in flight (no leaked file).
- **Non-owner promotion refused** (owner-only gate fires for member role too).
- Plus the existing mod/admin tests (`testChannelModeratorActionViaRoster` etc.) pass unchanged in behaviour.

Run focused: `cabal test simplex-chat-test --test-options='-m "roster"'`. Fast build: `cabal build --ghc-options=-O0`.

## Anchors (verified on revision; flag if they drift again)

- `xGrpRoster` handler: `Subscriber.hs:3167` (owner assert `3170`, relay version no-op `3179`, member no-op `3193-3194`, `setCachedGroupRoster` `3185`, `processRoster` `3202`, `currentPriv` `3207`). The review prompt's `3159/3162/3177` are ~8 lines high.
- `isRosterRole` `Internal.hs:1230`; `validateGroupRoster` `1234`; `buildGroupRoster` `1244`; `forwardCachedRoster` `1165`; `introduceInChannel` `1181` (uses `getGroupOwners` + `getGroupRosterMembers` at `1184-1185`); `bumpAndBroadcastRoster` `2145`; `sendGroupRosterToRelay` `2158`.
- `getGroupRosterMembers` `Store/Groups.hs:1214`; `getGroupOwners` `1222`; `getGroupModerators` `1203`.
- Promotion `Commands.hs:2725` (owner gate `2737`, cap `2739`, trigger `2746`); group delete `1270`, clear `1305`.
- Inline file: `sendFileInline_` `Internal.hs:1648`; `appendFileChunk` `1771` (in-place encrypt `1787-1794`); `startReceivingFile` `827`; `receiveFileChunk` `1301` (`RcvChunkFinal` `1319`, `getChatItemByFileId` `1329`); `bFileChunkGroup` `Subscriber.hs:2435`; `receiveInlineChunk` `2440`; `startRcvInlineFT` / `acceptRcvInlineFT` `Store/Files.hs:578/573`; `createRcvFileChunk` `656`; `getGroupFileIdBySharedMsgId` `310`.
- `files` schema `chat_schema.sql:254` (`chat_item_id` nullable `265`; `group_id ON DELETE CASCADE` `257`; no `shared_msg_id`, no file-type column). Tail migration `M20260526_group_roster` (`Migrations.hs`).
- **Anchor that did NOT match:** `encodeChatMessage` is in `Protocol.hs:909`, not `Internal.hs:2002`. The header-fits check uses `maxEncodedMsgLength` (`Protocol.hs:884`).
