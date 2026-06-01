# Roster: regular members + multipart delivery

Date: 2026-06-01. Extends Section 1 of `2026-05-26-public-groups-via-relays-unified.md`.

## Problem

Channels pin every subscriber to observer, so they can't post. We want owners to promote some subscribers to **regular members** who can post, and to support more named members than fit in one message. Today the roster (`XGrpRoster`) is an owner-signed snapshot of moderators + admins only, JSON-encoded, capped at 64 because it must fit a single message.

## The change in one sentence

The roster becomes the owner-signed list of **all promoted members** (member, moderator, admin), delivered as a small **signed JSON header** plus a **packed binary blob split into parts** — so it can hold hundreds and scale to thousands.

## Why a roster, and why members belong in it

The relay defaults every joiner to observer, and it forwards a member's posts only if its own record says that member's role is above observer (`memberCanSend`). Role is owner-asserted only — a relay must never be able to invent a poster. So a member who can post must be established in the owner-signed roster, exactly like a moderator. The roster is how the relay authorizes posting and how members learn each other's keys.

## Roster entry

`<memberId, key, role, privileges>`, binary-packed (no JSON/base64), ~53 bytes each.

- **No name.** A roster-created member gets a placeholder name derived from its id; its real profile arrives separately when it first posts. (Clients will hide unknown-member rows in channels — separate client work.)
- **privileges** — a reserved 16-bit field for future per-member capabilities (e.g. "post as the channel"). v1 only reserves it: always sent as 0, parsed and ignored. No storage, no behaviour yet.

## Delivery: signed header + binary parts

- **Header** = `XGrpRoster` (JSON, signed by the owner, relay-forwarded): version, SHA-256 of the blob, blob size. No member data inline.
- **Parts** = new `BGrpRosterPart` (binary, unsigned): version, part number, total parts, raw bytes.

The blob is the packed member list. The header's hash covers the **whole blob**, so the relay can re-split it however it likes. A receiver collects all parts, checks the hash against the signed header, then applies. **Nothing is applied until the hash matches** — no partial rosters.

Why this shape: only the small header needs a signature, and signing already works for JSON, so we avoid changing the signing wire format. The bulk stays raw binary instead of base64-bloated JSON. Up to ~280 members fit one part; beyond that the blob simply spans more parts through the same code path.

## Trust and safety (same model, wider set)

- The header must be **signed by an owner** (asserted in the handler) — a relay can't forge it or re-attribute it to a member whose key it controls.
- Parts are unsigned; their only integrity is the header's signed hash.
- A member's key is **trust-on-first-use**, pinned per id — a different key for a known id is rejected, never overwritten.
- **Members are now roster-gated**: a relay announcing a member who isn't in the roster is rejected (relay can't conjure a poster), and a member's role/key can't be changed by an unsigned relay message.
- **Anti-replay** preserved: older versions are rejected; equal version is a no-op.

## Storage and recovery

- The relay caches the signed header + the assembled blob, and re-sends them to joiners (re-splitting freely).
- Both relay and members **persist incoming parts in the database** and apply only when complete — so a restart mid-transfer resumes rather than losing progress.
- Apply + version bump + buffer cleanup happen in **one transaction**.

## One correctness point to keep in mind

"All promoted members" (the blob) and "the moderation set" are different things. The join-time introduction flow (`introduceInChannel`) must keep using **moderators/admins only**. If it used the full member list, every new subscriber would be announced to every member and every member introduced to every joiner — large traffic, and it breaks subscriber anonymity. Members learn about each other through the forwarded roster blob, not through individual introductions.

## Size cap

A cap (proposed 256, tunable) bounds the blob, the relay's cached copy, and the per-update work. It's enforced when the owner promotes. The cap is a product choice: at ≤~280 the roster is header + 1 part; above that it spans more parts. Pick it for the real target.

## Consequence to confirm

Because the roster is owner-signed, **only the owner can promote or demote members** (moderators cannot). This is consistent with the trust model but is a behaviour choice worth confirming.

## Known limitations (inherited, not introduced here)

- A malicious relay can withhold or corrupt a part, leaving a member on an older roster (it can drop any message regardless). Bounded by the documented new-joiner rollback gap.
- A just-promoted member's first posts may render as "unknown member" until the roster finishes arriving — self-healing on the next post.

## Out of scope

Granting/enforcing privileges; signing member message content; the joiner-role-on-profile change; iOS/Kotlin clients (including hiding unknown rows).

## Smaller alternative, if a first cut is preferred

Keep the roster JSON and single-message, capped ~110, just adding members + the reserved field. That ships "members in channels" for smaller channels with none of the multipart machinery; parts can be added later.
