# Channel owner unlimited delete

## Problem

Channel owners cannot delete content older than 24 hours. The limit makes sense in p2p groups (no authority, each member holds independent copy). In channels, the owner is the authority - their content, their right to remove it.

## Changes

### PR 1 - subscriber side (release first)

**Subscriber.hs:2168** - `rcvItemDeletable` check on `CIGroupRcv` path. When the sender has editorial role in a channel (`useRelays' gInfo && memberRole' mem >= GRModerator`), skip the time check.

**Subscriber.hs:1912** - `rcvItemDeletable` itself doesn't need to change - the caller skips it.

### PR 2 - owner side (release after subscribers update)

**Messages.hs:535** - `deletable'` returns `True` without time check when the item is in a channel where the user's role >= GRModerator. Needs group context threaded in, or a separate check at the call site.

**Commands.hs:790** - `assertDeletable` for `APIDeleteChatItem` - skip time check for channel editorial roles (>= GRModerator).

**UI (iOS + Kotlin)** - remove the "delete for me / delete for everyone" question for channel owners/admins/moderators. In channels, editorial deletion is always for everyone. The "delete for me only" option makes no sense for a publication.

### Strings

`group_members_can_delete_channel` - "(24 hours)" should note that this limit applies to subscribers, not owners. Owners can delete at any time.

Other `(24 hours)` strings are for direct chats and p2p groups - unchanged.

### Scope

In channels, owners/admins/moderators (role >= GRModerator) get unlimited delete of their own content - they are the editorial team. Members/subscribers stay on 24-hour limit. Moderation of others' content is already unlimited (separate code path, no time check).

## Release order

1. PR 1 ships. Old owners can't send old deletions yet, so nothing changes for subscribers. New subscribers are ready.
2. PR 2 ships. Owners can now delete old content. New subscribers honor it. Old subscribers silently ignore (message stays - acceptable degradation, not an error after PR 1).
