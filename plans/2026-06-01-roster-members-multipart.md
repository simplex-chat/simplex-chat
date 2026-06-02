# Roster: regular members + larger rosters via inline file

Date: 2026-06-01. Extends Section 1 of `2026-05-26-public-groups-via-relays-unified.md`.

## Problem

Channels pin every subscriber to observer, so they can't post. We want owners to promote some subscribers to **regular members** who can post, and to support more named members than fit in one message. Today the roster (`XGrpRoster`) is an owner-signed snapshot of moderators + admins only, JSON-encoded, capped at 64 because it must fit a single message.

## The change in one sentence

The roster becomes the owner-signed list of **all promoted members** — member, moderator, admin (owners stay on the link, never in the roster) — delivered by reusing the existing **inline file transfer**: a small signed `XGrpRoster` header carries the blob's size and digest, and the packed binary member list rides the file-chunk machinery. It scales to a few thousand members.

## Why a roster, and why members belong in it

The relay defaults every joiner to observer, and it forwards a member's posts only if its own record says that member's role is above observer (`memberCanSend`). Role is owner-asserted only — a relay must never be able to invent a poster. So a member who can post must be established in the owner-signed roster, exactly like a moderator. The roster is how the relay authorizes posting and how members learn the keys and roles of the promoted set. The owner already learns each subscriber's key when they join, so promoting one needs no new key exchange.

## Roster entry

`<memberId, key, role, privileges>`, binary-packed (no JSON/base64), ~53 bytes each.

- **No name.** A roster-created member gets a placeholder name derived from its id; its real profile arrives separately when it first posts. (Clients will hide unknown-member rows in channels — separate client work.)
- **privileges** — a reserved 16-bit field for future per-member capabilities (e.g. "post as the channel"). v1 only reserves it: always sent as 0, parsed and ignored. No storage, no behaviour yet.

## Delivery: reuse the inline file transfer

- **Header** = `XGrpRoster` (JSON, signed by the owner, relay-forwarded): version + an `InlineFileInvitation { fileSize, fileDigest }` — a lean `FileInvitation` with no `fileName` / `fileConnReq` / `fileInline` / `fileDescr` (it is always an inline send). No member data inline.
- **Blob** = the packed member list, sent as `BFileChunk` against the `XGrpRoster` message's `shared_msg_id`, reusing the existing inline send/receive, chunk tracking, and restart recovery.
- The file machinery owns chunking, completion, and recovery. We add the one thing it lacks: **verify the assembled blob against the signed `fileDigest`** on completion. That is the integrity gate — a relay can corrupt chunks, but the owner-signed digest catches it. (Inline receive does not check the digest today; reuse the existing `FileDigest`.)

Why reuse: the inline transfer already persists partial chunks and resumes after a restart, so we don't reinvent that. The signature stays on the small JSON header (the existing signing path), and the bulk stays raw binary.

## Keeping it off the chat UI (the key adaptation)

An inline file is normally a user-facing file attached to a chat item. To carry an internal roster blob:

- **`shared_msg_id` on the `files` table**, so a roster file is found directly by the `XGrpRoster` message id. Today a file is only reachable *through* its chat item; a roster file has no chat item (`chat_item_id` NULL), so it needs the direct key.
- **A file-type tag on `files`** marking a roster file. It auto-accepts, never surfaces in the UI, and on completion routes to: save the blob on the group, delete the file, apply the roster — instead of becoming a received-file chat item. (The on-disk file still needs a name; generate one from the message id.)

## Trust and safety (same model, wider set)

- The header must be **signed by an owner** (asserted in the handler) — a relay can't forge it or re-attribute it to a member whose key it controls.
- Chunks are unsigned; their only integrity is the header's signed digest, **verified on completion**.
- A member's key is **trust-on-first-use**, pinned per id — a different key for a known id is rejected, never overwritten.
- **Members are now roster-gated**: a relay announcing a member who isn't in the roster is rejected (relay can't conjure a poster), and a member's role/key can't be changed by an unsigned relay message.
- **Anti-replay** preserved: older versions are rejected; equal version is a no-op.

## Storage, relay, recovery

- The blob transits through an on-disk file (the inline machinery's working store) — that's what gives restart recovery for free. On completion the assembled blob is **saved on the group as the durable copy**, and the file is deleted.
- The relay re-serves joiners by **re-sending the cached group blob as an inline file** (drive the inline send from the stored blob — via a temp file or a send-from-bytes path; the current send reads from a file). It still forwards the owner's signed `XGrpRoster` verbatim, so chunks reference the same id and the joiner verifies the same signed digest.
- A **newer version** (new `XGrpRoster` → new `shared_msg_id` → new file) supersedes any in-flight older transfer; stale roster files are cleaned up.
- Apply + version bump happen in **one transaction**.

## One correctness point to keep in mind

"All promoted members" (the blob) and "the moderation set" are different things. The join-time introduction flow (`introduceInChannel`) must keep using **moderators/admins only**. If it used the full member list, every new subscriber would be announced to every member and every member introduced to every joiner — large traffic, and it breaks subscriber anonymity. Members learn the promoted set — ids, roles, keys — from the roster blob, and each other's profiles when they post, not through individual introductions.

## Watch: the file-type tag cross-cuts the file lifecycle

Every file path — accept, complete, cancel, delete, cleanup, and the file-list queries — must tolerate a roster file with a NULL `chat_item_id`, so it can't leak into a file list or break a deletion. This is the main cost of reusing the file machinery, and where bugs would hide.

## Size cap and ceiling

- A roster **cap** (proposed 256, tunable) bounds the blob, the cached copy, and the per-update work; enforced when the owner promotes.
- Inline files have a built-in ceiling (`offerChunks` / `receiveChunks` ≈ 126–236 KB ≈ ~2,000–4,000 members at ~53 B each). Roster files either inherit it or are exempted — if exempted, the cap is the only size bound. The inline route doesn't suit tens of thousands; keep the target in the low thousands.

## Consequence to confirm

Because the roster is owner-signed, **only the owner can promote or demote members** (moderators cannot). This is consistent with the trust model but is a behaviour choice worth confirming.

## Known limitations (inherited, not introduced here)

- A malicious relay can withhold or corrupt chunks, leaving a member on its last-applied roster — the same staleness a relay can impose by dropping any message. Existing members are still protected from *rollback* by the version check; the separate new-joiner rollback gap is documented in the channels overview.
- A just-promoted member's first posts may render as "unknown member" until the roster file finishes arriving — self-healing on the next post.

## Out of scope

Granting/enforcing privileges; signing member message content; the joiner-role-on-profile change; iOS/Kotlin clients (including hiding unknown rows).

## Possible optimization (not in v1)

Even a small mod-only roster now goes through the file transfer (header + a chunk + an on-disk file), heavier than today's single signed message. If that churn matters, a size threshold could keep small rosters inline in `XGrpRoster` and use the file only above it — at the cost of a second code path. v1 keeps the single uniform file path.
