# Clear chat resets the support chat counters of a group

## Problem

Clear chat on a group removes its chat items. The per-member support chat counters (unread, attention, mentions) and the group's count of members that require attention stay as they were.
The chat list row, the group info "Chat with admins" row and the "chats with members" bar keep a badge for items that no longer exist, and no screen can clear it: opening the support chat shows no items to read.

## Cause

`deleteGroupChatItemsMessages` (Store/Messages.hs) deletes `messages`, `chat_item_reactions` and `chat_items`, and does not touch `group_members.support_chat_items_*` or `groups.members_require_attention`.
`APIClearChat` (Library/Commands.hs) returns the `GroupInfo` read before the deletions, so the open app replaces its chat info with the old counters (`ChatModel.clearChat` takes `chatInfo` from the response).
`deleteGroupChatItemsMessages` has one caller, the group branch of `APIClearChat`.

## Fix

1. `deleteGroupChatItemsMessages`: set `support_chat_items_unread`, `support_chat_items_member_attention` and `support_chat_items_mentions` to 0 for all members of the group, including the own membership row.
2. Same function, before step 1: decrease `groups.members_require_attention` by the number of members other than the user that are not pending and have support chat attention or mentions above 0, not below 0.
   These are the members for which `gmRequiresAttention` changes from true to false when their counters become 0; pending members keep requiring attention, so their part of the counter stays.
   A recount of pending members is wrong: a pending invitee is counted only when it connects (Subscriber.hs CON handler), so a recount before that counts it twice.
   The own membership row is excluded because the counter is only changed for other members (Subscriber.hs `increaseGroupMembersRequireAttention` call sites, `updateGroupMembersRequireAttention` callers).
3. `APIClearChat`: read the group info after the deletions and return it in `CRChatCleared`.

`support_chat_ts` is not changed: the support chat row stays, with all counters 0, like an empty chat.
The decrease uses `MAX` on SQLite and `GREATEST` on Postgres, like `decreaseGroupMembersRequireAttention`; enum values are bound like other `member_status` queries.
`chat_query_plans.txt` has two new entries, generated with `explain query plan` on SQLite with foreign keys on; the harness reproduced the existing entries of the neighbouring statements byte for byte first.

## Alternatives

- Reset `members_require_attention` to 0: wrong while members are pending review.
- Recount the pending members: double counts pending invitees that have not connected yet.
- Update the counters in `APIClearChat` instead of the store function: the store function is the single place that knows what clearing removes; one caller either way.
- Do not re-read the group info and patch the old value in memory: duplicates the decrease in Haskell.
- Delete the support chats (as removing a member does): clears `support_chat_ts`, the row would disappear from the app after Clear chat.

## Tests

`testScopedSupportUnreadStatsOnClearChat` (tests/ChatTests/Groups.hs): a member writes to the support chat, the owner replies, both have unread counters; both clear the chat; `/member support chats #team` shows `members require attention: 0` and all counters 0 for the owner, and 0 counters for the member (own membership).
Not written: pending-member case (needs admission review setup).
Not run locally (no Haskell build or test run); CI only.

## Open questions

- The UI does not need a change: the response now carries the new counters and the clients already replace the chat info with it.
- Other paths that mark items read without touching the counters (for example `/read user`) were not examined.
