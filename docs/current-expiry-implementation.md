# Hard Expiry Implementation Summary

This document describes the hard expiry (wall-clock message deletion) feature added to this fork of SimpleX Chat.

---

## What This Fork Adds

A **hard expiry** mechanism for group messages: an absolute wall-clock deletion timestamp stamped on every message at send time. When the timestamp passes, every client deletes the message regardless of whether it was ever read.

This solves the dead-member problem: if a group member loses their device or dies, unread messages persist indefinitely under the existing read-triggered disappearing messages system. Hard expiry provides a backstop that fires on wall-clock time alone.

---

## How It Works

1. **Group-level setting**: `hardExpiryDuration` (seconds) is stored on `TimedMessagesGroupPreference`, alongside the existing read-based `ttl`. Default is 604800 seconds (7 days) when enabled.

2. **Sender stamps messages**: At send time, the sender computes `hardExpiryAt = brokerTimestamp + hardExpiryDuration` and includes it in the message's `ExtMsgContent` JSON and in the local `CITimed` record.

3. **Wire protocol**: The `hardExpiryAt` field is transmitted as a new optional JSON field on `ExtMsgContent`. Old clients ignore it (decodes as `null`).

4. **Local storage**: `hard_expiry_at TEXT` column on `chat_items`, indexed on `(user_id, hard_expiry_at)`.

5. **Sweep deletion**: The existing `cleanupManager` periodic loop queries for items where `hard_expiry_at <= now` and deletes them in bulk, before UI renders unread counts.

6. **Setting propagation**: Changes to `hardExpiryDuration` propagate to all group members via the existing `XGrpInfo`/`XGrpPrefs` mechanism. Only future messages are affected; already-sent messages keep their original expiry timestamp.

---

## What Was NOT Changed

The existing **disappearing messages** system is untouched. It continues to work as before:

- `timed_ttl` stores a duration in seconds per message.
- `timed_delete_at` is set only when a received message is marked as read (`timed_delete_at = now + ttl`).
- Per-item timer threads handle deletion after the read-triggered deadline.

This is the "read-triggered timer." Hard expiry does not modify, replace, or interfere with it.

---

## How the Two Timers Interact

| Timer | Trigger | Stored as | Set when |
|---|---|---|---|
| Disappearing messages (read-triggered) | Message is read | `timed_delete_at` | On read |
| Hard expiry (wall-clock) | Clock reaches deadline | `hard_expiry_at` | At send time |

Both timers run independently. **Whichever fires first wins** — the message is deleted by the first timer to reach its deadline. They are complementary:

- Read-triggered expiry handles the normal case (messages read promptly).
- Hard expiry handles the failure case (messages never read).

---

## CLI Commands Added

| Command | Effect |
|---|---|
| `/set expiry #groupName 7d` | Set hard expiry duration for future messages (accepts `1h`, `6h`, `12h`, `1d`, `3d`, `7d`, `30d`, or seconds) |
| `/set expiry #groupName off` | Disable hard expiry for future messages |
| `/show expiry #groupName` | Display current hard expiry setting for the group |

---

## Files Modified

### New files
- `src/Simplex/Chat/Store/SQLite/Migrations/M20260412_hard_expiry.hs` — SQLite migration (add column + index)
- `src/Simplex/Chat/Store/Postgres/Migrations/M20260412_hard_expiry.hs` — Postgres migration

### Modified files
- `src/Simplex/Chat/Types/Preferences.hs` — Added `hardExpiryDuration` field to `TimedMessagesGroupPreference`
- `src/Simplex/Chat/Messages.hs` — Added `hardExpiryAt` field to `CITimed`; added `groupHardExpiryDuration` helper
- `src/Simplex/Chat/Protocol.hs` — Added `hardExpiryAt` field to `ExtMsgContent` (wire format)
- `src/Simplex/Chat/Store/Messages.hs` — Updated all chat item queries/inserts/updates for `hard_expiry_at`; added `getHardExpiredItems`
- `src/Simplex/Chat/Library/Commands.hs` — CLI parser for `/set expiry` and `/show expiry`; `cleanupHardExpiredItems` in cleanup manager
- `src/Simplex/Chat/Library/Subscriber.hs` — Receiver-side: extract `hardExpiryAt` from incoming messages, populate `CITimed`
- `src/Simplex/Chat/Library/Internal.hs` — Sender-side: compute `hardExpiryAt` at send time from group setting
- `src/Simplex/Chat/Controller.hs` — Added `SetGroupHardExpiry` and `ShowGroupHardExpiry` chat commands
- `src/Simplex/Chat/Store/SQLite/Migrations.hs` — Registered SQLite migration
- `src/Simplex/Chat/Store/Postgres/Migrations.hs` — Registered Postgres migration
- `src/Simplex/Chat/Store/SQLite/Migrations/chat_schema.sql` — Updated schema
- `src/Simplex/Chat/Store/Postgres/Migrations/chat_schema.sql` — Updated schema
