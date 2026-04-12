# PR Proposal: Add hard expiry (wall-clock message deletion) for groups

## Motivation

SimpleX currently has (a) **disappearing messages** that fire on read, and (b) **delete-for-everyone** that fires on demand. Both require something to happen — a read event, or a button press. Neither handles the case where a device is unreachable and nothing happens.

This PR adds a third option: a **send-time-stamped hard expiry** that fires on local clock regardless of read state or user action.

The concrete trigger: a member of a private group dies. Messages sent after their death remain unread on their device indefinitely. No existing mechanism cleans them up. Hard expiry provides the backstop — old messages are deleted when the clock reaches their deadline, whether or not the app is opened, the message is read, or anyone presses a button.

## Design

The feature introduces two concepts:

1. **`hardExpiryDuration`** — a group-level setting (seconds) stored on `TimedMessagesGroupPreference`. Admins set this via group preferences. Default: 604800 seconds (7 days). Range: 1 hour to 30 days. Can be disabled (`Nothing`).

2. **`hardExpiryAt`** — a per-message absolute UTC timestamp. Computed by the sender as `brokerTimestamp + hardExpiryDuration` and transmitted to all recipients in the message JSON. Each client stores it locally and deletes the message when the clock reaches that time.

The setting propagates to all group members through the existing `XGrpInfo`/`XGrpPrefs` mechanism. Changing the setting affects only future messages — already-sent messages retain their original expiry timestamp.

## How it coexists with disappearing messages

The two systems are independent and complementary:

| | Disappearing messages | Hard expiry |
|---|---|---|
| **Trigger** | Message is read | Wall clock reaches deadline |
| **Timer starts** | On read | At send time |
| **Stored as** | `timed_delete_at` (set on read) | `hard_expiry_at` (set on send) |
| **Scope** | Per-message, group or direct | Per-message, groups only |

**Whichever fires first wins.** If a message is read promptly, the read-triggered timer will likely delete it before the hard expiry. If the message is never read, the hard expiry guarantees deletion anyway.

No existing disappearing messages behavior was changed.

## Implementation summary

The change is minimal — approximately 300 lines of logic across the following areas:

### Database
- New `hard_expiry_at TEXT` column on `chat_items`, indexed on `(user_id, hard_expiry_at)`.
- Migration provided for both SQLite and Postgres.

### Types
- `TimedMessagesGroupPreference` gains `hardExpiryDuration :: Maybe Int` (seconds).
- `CITimed` gains `hardExpiryAt :: Maybe UTCTime` (local storage).
- `ExtMsgContent` gains `hardExpiryAt :: Maybe UTCTime` (wire protocol).

### Send path
- When sending a group message, if the group has `hardExpiryDuration` set, the sender computes `hardExpiryAt = brokerTimestamp + duration` and includes it in `ExtMsgContent` and the local `CITimed`.

### Receive path
- The receiver extracts `hardExpiryAt` from the incoming `ExtMsgContent` JSON and stores it in `CITimed` and the `hard_expiry_at` column.

### Sweep
- `cleanupHardExpiredItems` is called in the existing `cleanupManager` periodic loop. It queries `SELECT ... FROM chat_items WHERE hard_expiry_at IS NOT NULL AND hard_expiry_at <= ?` and deletes matching items. Runs before the UI renders unread counts.

### CLI
- `/set expiry #groupName <duration|off>` — set or disable hard expiry for a group.
- `/show expiry #groupName` — display current setting.

## Backward compatibility

- **Old clients receiving messages with `hardExpiryAt`**: The field is optional JSON (`.:? "hardExpiryAt"`). Old clients that do not know this field will decode it as `null` and ignore it. The message is delivered and displayed normally; it simply will not have a wall-clock expiry on that client.
- **New clients receiving messages from old clients**: Old clients do not set `hardExpiryAt`, so it arrives as `null`. The message has no hard expiry. This is correct — the sender's client did not support the feature.
- **Mixed groups**: Messages from updated clients will be hard-expired by other updated clients. Messages from old clients will not. This is a safe, progressive rollout.
- **No breaking changes** to the existing SMP or chat protocol. No new message types. No changes to encryption or signing.

## What is NOT included (left for the SimpleX team)

- **iOS/Android UI**: Group creation picker for hard expiry duration, group info display of current setting, per-message expiry indicator badge. The backend is fully wired; only UI integration is needed.
- **Direct message hard expiry**: This implementation is group-only. Extending to direct messages would be straightforward but was out of scope.
- **Local read TTL concept**: Initially considered as a separate per-conversation setting, but dropped — the existing disappearing messages feature already provides group-level read-triggered expiry, making a separate mechanism redundant.

## Testing

Built and tested via the SimpleX CLI (`simplex-chat`):

- Verified `hard_expiry_at` is stamped on group messages when the setting is enabled.
- Verified changing the group setting (e.g., from 7 days to 1 day) affects only future messages; old messages retain their original expiry.
- Verified `/set expiry #group off` disables hard expiry for future messages.
- Verified `/show expiry #group` displays the current duration.
- Verified the cleanup sweep deletes expired items on schedule.
- Verified messages from a client without hard expiry support are received normally (no hard expiry, no errors).

## Files changed

| File | Change |
|---|---|
| `src/Simplex/Chat/Types/Preferences.hs` | Added `hardExpiryDuration` to `TimedMessagesGroupPreference` |
| `src/Simplex/Chat/Messages.hs` | Added `hardExpiryAt` to `CITimed`; added `groupHardExpiryDuration` helper |
| `src/Simplex/Chat/Protocol.hs` | Added `hardExpiryAt` to `ExtMsgContent` wire format |
| `src/Simplex/Chat/Store/Messages.hs` | Updated queries/inserts/updates; added `getHardExpiredItems` |
| `src/Simplex/Chat/Library/Commands.hs` | CLI commands; sweep integration in `cleanupManager` |
| `src/Simplex/Chat/Library/Subscriber.hs` | Receiver-side `hardExpiryAt` extraction |
| `src/Simplex/Chat/Library/Internal.hs` | Sender-side `hardExpiryAt` computation |
| `src/Simplex/Chat/Controller.hs` | `SetGroupHardExpiry` / `ShowGroupHardExpiry` command types |
| `src/Simplex/Chat/Store/SQLite/Migrations/M20260412_hard_expiry.hs` | SQLite migration |
| `src/Simplex/Chat/Store/Postgres/Migrations/M20260412_hard_expiry.hs` | Postgres migration |
| `src/Simplex/Chat/Store/SQLite/Migrations.hs` | Migration registration |
| `src/Simplex/Chat/Store/Postgres/Migrations.hs` | Migration registration |
| `src/Simplex/Chat/Store/*/Migrations/chat_schema.sql` | Updated schemas |
