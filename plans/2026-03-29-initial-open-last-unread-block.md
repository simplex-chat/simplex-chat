# Initial chat open: jump to last unread block

## Problem

When opening a chat with unread messages, the app always scrolls to the oldest unread message (`minUnreadItemId`). For casual group members with hundreds of unreads, this forces them to scroll through the entire backlog to reach new messages.

The bottom circle scrolls to the latest messages without marking all as read (by design — moderators use this to reply quickly, then return to the top circle to read sequentially). But the next time the chat opens, it jumps back to the oldest unread.

Users want the initial open to skip old unreads and land on the "new" ones — messages that arrived after their last interaction.

## Design

Change `CPInitial` to use a different pivot for `getDirectChatAround'` / `getGroupChatAround'`.

Currently the pivot is `minUnreadItemId` (absolute first unread). Instead, try `maxViewedItemId` (last non-unread item in sort order) first. If not found, fall back to `minUnreadItemId`. The `getAround'` function is unchanged — it always loads `CRBefore`/`CRAfter` around the pivot and includes it.

**maxViewedItemId**: the last item in sort order that is not `CISRcvNew`.
- "Viewed" means received read or sent — any `item_status != CISRcvNew`.

### Why include works for both pivots

`getDirectChatAround'` always includes the pivot in the result. When the pivot is maxViewed (a read item), including it adds one extra read item — harmless. The client scrolls to the first unread in the loaded items, which is the first item in `afterCIs` (the new unreads after the gap). The include/exclude distinction is unnecessary.

### Sort order

- Groups: `(item_ts, chat_item_id)`
- Direct chats: `(created_at, chat_item_id)`

### Cases

Items in display order (left = oldest/top, right = newest/bottom). U = unread (`CISRcvNew`), R = not unread (read, sent, event).

**Case 1: Unreads contiguous from bottom, no gap**
```
R...R  U...U
    ↑ maxViewed (last R) used as pivot
```
`afterCIs` = the unreads. Same as current behavior.

**Case 2: Gap, then new unreads at bottom**
```
R...R  U...U  R...R  U...U
                  ↑ maxViewed used as pivot
```
`afterCIs` = new unreads only. Skips old unreads. This is the desired improvement.

**Case 3: Gap at bottom, no new unreads**
```
R...R  U...U  R...R
                  ↑ maxViewed used as pivot
```
`afterCIs` = empty. Items loaded are the latest. Old unreads reachable via top circle.

**Case 4: All unread**
```
U...U
```
maxViewed = NULL. Fall back to `minUnreadItemId` as pivot. Current behavior.

**Case 5: No unreads**

`maxViewedItemId` returns some item but `minUnreadItemId` returns `Nothing` — no unreads exist. Handled by stats showing zero unreads. `getAround'` loads items around maxViewed, which are the latest items.

Actually: maxViewed is always found when items exist (every chat has at least sent items or read items unless case 4). So the flow is: maxViewed found → load around it → stats show 0 unreads → client shows latest items.

### No UI changes needed

Only `CPInitial` backend logic changes. The top circle, unread counter, unread separator, and all pagination continue to use `minUnreadItemId` as before.

### Out-of-order delivery

A late-arriving group message with old `item_ts` but recent `created_at` sorts into the old unread block in display order and gets skipped on initial open. This is acceptable — the top circle still reaches it.

### Notes (local chat)

Notes (`getLocalChatInitial_`) have unread handling in code but it's dead — all items are sent, `CISRcvNew` never occurs. No change needed.

### Open concern

In case 2, `CRBefore(maxViewed)` loads items before the gap, which may include old unreads. The client's scroll logic finds the first unread in loaded items (`lastIndex(where: hasUnread)` in reversed list), which could be an old unread from `beforeCIs` rather than a new unread from `afterCIs`. To be validated during testing — if problematic, may need client-side adjustment or limiting `beforeCIs` count.

## Implementation

### Files to modify

`src/Simplex/Chat/Store/Messages.hs` — all changes are here.

### New functions

#### Direct chats

```haskell
-- max viewed item: received read or sent (any item_status != CISRcvNew)
getContactMaxViewedItemId_ :: DB.Connection -> User -> Contact -> IO (Maybe ChatItemId)
```

Query:
```sql
SELECT chat_item_id
FROM chat_items
WHERE user_id = ? AND contact_id = ? AND item_status != ?
ORDER BY created_at DESC, chat_item_id DESC
LIMIT 1
```

#### Groups

```haskell
-- max viewed item: received read or sent (any item_status != CISRcvNew)
getGroupMaxViewedItemId_ :: DB.Connection -> User -> GroupInfo -> Maybe GroupChatScopeInfo -> Maybe MsgContentTag -> ExceptT StoreError IO (Maybe ChatItemId)
```

Mirrors `queryUnreadGroupItems` structure but with `item_status != ?` instead of `item_status = ?`. Handles the same 4-case scope/content filter dispatch. New function `queryViewedGroupItems`.

Query (for the no-scope, no-content-filter case):
```sql
SELECT chat_item_id
FROM chat_items
WHERE user_id = ? AND group_id = ?
  AND group_scope_tag IS NULL AND group_scope_group_member_id IS NULL
  AND item_status != ?
ORDER BY item_ts DESC, chat_item_id DESC
LIMIT 1
```

### Modified functions

#### `getDirectChatInitial_`

Current:
```haskell
getDirectChatInitial_ db user ct contentFilter count = do
  liftIO (getContactMinUnreadId_ db user ct) >>= \case
    Just minUnreadItemId -> do
      unreadCount <- liftIO $ getContactUnreadCount_ db user ct
      let stats = emptyChatStats {unreadCount, minUnreadItemId}
      getDirectChatAround' db user ct contentFilter minUnreadItemId count "" stats
    Nothing -> (,Just $ NavigationInfo 0 0) <$> getDirectChatLast_ db user ct contentFilter count ""
```

New — only the pivot source changes, rest stays the same:
```haskell
getDirectChatInitial_ db user ct contentFilter count = do
  liftIO (getContactMaxViewedItemId_ db user ct >>= maybe (getContactMinUnreadId_ db user ct) (pure . Just)) >>= \case
    Just pivotId -> do
      unreadCount <- liftIO $ getContactUnreadCount_ db user ct
      minUnreadItemId <- fromMaybe 0 <$> liftIO (getContactMinUnreadId_ db user ct)
      let stats = emptyChatStats {unreadCount, minUnreadItemId}
      getDirectChatAround' db user ct contentFilter pivotId count "" stats
    Nothing -> (,Just $ NavigationInfo 0 0) <$> getDirectChatLast_ db user ct contentFilter count ""
```

#### `getGroupChatInitial_`

Same minimal change — only the pivot source:
```haskell
getGroupChatInitial_ db user g scopeInfo_ contentFilter count = do
  (getGroupMaxViewedItemId_ db user g scopeInfo_ contentFilter >>= maybe (getGroupMinUnreadId_ db user g scopeInfo_ contentFilter) (pure . Just)) >>= \case
    Just pivotId -> do
      stats <- getGroupStats_ db user g scopeInfo_
      getGroupChatAround' db user g scopeInfo_ contentFilter pivotId count "" stats
    Nothing -> do
      stats <- liftIO $ getStats 0 (0, 0)
      (,Just $ NavigationInfo 0 0) <$> getGroupChatLast_ db user g scopeInfo_ contentFilter count "" stats
  where
    getStats minUnreadItemId (unreadCount, unreadMentions) = do
      reportsCount <- getGroupReportsCount_ db user g False
      pure ChatStats {unreadCount, unreadMentions, reportsCount, minUnreadItemId, unreadChat = False}
```

### Summary of all affected functions

| Function | Change |
|----------|--------|
| `getDirectChatInitial_` | Try maxViewed first, fall back to minUnread, same `getAround'` call |
| `getGroupChatInitial_` | Same |
| **New:** `getContactMaxViewedItemId_` | MAX non-unread by (created_at DESC, id DESC) |
| **New:** `getGroupMaxViewedItemId_` | MAX non-unread with scope/filter dispatch |
| **New:** `queryViewedGroupItems` | Like `queryUnreadGroupItems` but `item_status != ?` |

Nothing else changes. `getDirectChatAround'`, `getGroupChatAround'`, `getDirectChatAround_`, `getGroupChatAround_`, `getContactMinUnreadId_`, `getGroupMinUnreadId_`, `getContactStats_`, `getGroupStats_`, `getChatItemIDs`, `NavigationInfo` computation, mark-read operations, UI code — all unchanged.

### Performance

- `maxViewedItemId` query: scans backward from the largest sort key, skipping unread items. Fast when there are recent read/sent items (the common case). Worst case: all items are unread — returns `Nothing` and falls back to minUnread.

- All other queries are existing code, same performance.

- Queries run only during `CPInitial` (chat open). No writes.

### Testing

1. Open chat with unreads, no prior interaction → same as current (case 1/4)
2. Open chat, jump to bottom (marks bottom screen read), close, reopen → lands on new unreads after the gap (case 2)
3. Open chat, jump to bottom, reply, close, new messages arrive, reopen → lands on new messages (case 2)
4. Open chat, jump to bottom, close, no new messages, reopen → loads around last viewed item at bottom (case 3)
5. Open chat, read from top (marks first screen read), close, reopen → lands on next unread after first screen, same as current (case 1)
6. Group with scope (member support chat) → same behavior with scope filter applied
7. Group with content filter (reports) → same behavior with content filter applied
