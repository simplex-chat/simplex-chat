# Group navigation info counts the items the chat lists

## Problem
A member support chat, opened while other members' support chats have newer unread messages, requests
`/_get chat #1(_support:2) after=28 count=100` in a loop (`items=[]`): 169 in about 6 s, master, 40k members.

## Cause
- `Store/Messages.hs:1701` (master): `getGroupChatAround'` calls `getGroupNavInfo_` without scope or filter.
- `:1781-1830`: the four COUNT sub-queries filter on `user_id`, `group_id` (and `item_status`) only, so the
  main chat also counted newer support items. `queryUnreadGroupItems` (`:1757-1779`) filters by scope.
- Bob's chat contains items 14, 16, 17, 26-28; 29-35 are others' unread support items; the core returned 7/7.
Client: `ChatItemsLoader.kt:65` adds a split when `afterTotal > 0`; an empty `After` returns early (`:38-40`),
so the split stays. `PreloadItems` exists only while `!loadingMoreItems` (`ChatView.kt:2802`); each request
toggles it (`:1888-1898`), the effect restarts and `preloadItemsAfter` (`:2865`) requests the same page.

## Fix
- `withGroupItemsCond` contains the one `(scope, contentFilter)` dispatch for `getChatItemIDs`,
  `queryUnreadGroupItems` and `getGroupNavInfo_`; it passes condition and parameters to a rank-2 continuation:
  main chat `group_scope_tag IS NULL AND group_scope_group_member_id IS NULL` (existing text; Postgres btree
  indexes); support chat `group_scope_tag = ? AND group_scope_group_member_id IS NOT DISTINCT FROM ?` (admins'
  own chat: NULL); link `has_link = 1`; other tags (incl. Report) `msg_content_tag = ?`; scope with filter
  `SEInternalError`. Only Initial/Around return navigation info; in practice only the Reports view sends a
  filter with them (media filters use Last/After/Before); the fix does not depend on this.
- `getGroupNavInfo_` takes scope and filter, runs in `ExceptT`, counts the set `getChatItemIDs` lists (search
  text aside); one `countAfter`, with and without `AND item_status = ?`. No client, schema or API change.
- Link filter in `queryUnreadGroupItems`: `has_link = 1` (was `msg_content_tag = ?`); other texts unchanged.
- Two `chat_query_plans.txt` entries replaced (covering `idx_chat_items_group_scope_item_status`/`_item_ts`).

## Approaches considered
- Kotlin: drop the split on an empty `After` (~4 lines): client still gets wrong counts; possible on top.
- Kotlin: `ignoreLoadingRequests` guard for `After`: same trade-off, more state.
- Kotlin: keep the effect composed, check `loadingMoreItems` inside: changes pagination timing for every chat.
- Scope condition only when `contentFilter` is `Nothing`: a third dispatch; Reports view counts every item.
- Scope-only counts: Reports view counts main-chat items but lists support-scope reports.
- Filter the unscoped counts in Haskell: not possible without loading the other items.
- Chosen: the shared condition, so list, unread queries and counts select the same item set.

## Tests
- Desktop GUI, 40,000-member fixture, 8 s in each of 4 support chats, `apiGetChat` lines in the app log:
  master 135, 141, 1, 141 (436 in the run), PR 1 each (5 with the group chat). Videos in the PR body.
- Bisection: master 169 in 6 s, master Kotlin + PR #31 core 33 in 6 s, core fix alone 0, `navInfo=(0,0)`.
- Reports view, small group, 6 reports, before and after newer main-chat messages: 1 load in 8 s on master and
  the fix; `unreadCount` stays unfiltered, as before; master miscounting (or looping) there is inferred.
- No Haskell test added: `chat` output lacks `NavigationInfo`, no helper decodes the `/_get chat` JSON.
- Query plans: Python sqlite 3.40.1 on the repo schema (reproduces every existing SELECT entry byte for byte),
  not the CI test; tests execute only the main-scope variants. Not run: Haskell tests (CI only), Android, iOS,
  link filter in the GUI, Postgres path (`IS NOT DISTINCT FROM` is standard in both).

## Open questions
- Keep the shared condition, or apply the scope only when `contentFilter` is `Nothing` (smaller diff)?
- Keep the link-filter change in `queryUnreadGroupItems` here, or move it to a separate PR?
- Add the client-side safeguard (drop the split on an empty `After`) as a separate PR?
- Whether the UI displays the previously wrong `afterUnread` for support chats was not checked.
- Unexplained: ~40 ms per `After` request at 40k members; it only affects whether the loop stops by chance.
- Header shows the previous member's messages: separate master defect, `nd/support-chat-modal-state`.
