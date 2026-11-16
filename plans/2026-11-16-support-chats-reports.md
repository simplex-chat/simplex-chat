# Member support chats and reports

## Problems

1. A member support chat keeps reloading while it is open, when other members' support chats have newer messages: the app requests the same empty page about 25 times a second.
2. After Clear chat on a group, the "Chats with members" list, the group's row and "Chat with admins" keep badges for support messages that no longer exist.
3. After "Remove member and delete messages", reports about the removed member stay active, and the reports count stays above the number of listed reports.

## Fixes

| Problem | Source | Change |
|---|---|---|
| 1 | https://github.com/Narasimha-sc/simplex-chat/pull/59 | `getGroupNavInfo_`, `getChatItemIDs` and `queryUnreadGroupItems` share one item-set condition, `withGroupItemsCond`, so the counts of newer items cover only the items the chat lists |
| 2 | https://github.com/Narasimha-sc/simplex-chat/pull/67 | `deleteGroupChatItemsMessages` sets the members' support chat counters to 0 and lowers `members_require_attention`; `APIClearChat` returns the group info read after the deletion |
| 3 | https://github.com/simplex-chat/simplex-chat/pull/7906 | `markMemberReportsDeleted` marks reports about and by the removed member deleted, and `CEvtGroupChatItemsDeleted` lists them |

All three change only the core and `chat_query_plans.txt`; the client code is unchanged. Problem 3 needed https://github.com/simplex-chat/simplex-chat/pull/7908 in the clients, merged on 2026-11-05.

## Tested

- 1: desktop, 40,000-member fixture: 460 requests on master, 5 with the fix.
- 2: test `testScopedSupportUnreadStatsOnClearChat`; desktop run of the first version.
- 3: test `should archive reports about the member removed with messages and update the reports count`; desktop and Android runs.
- Fork CI of a combined branch with 1 and 2: SQLite 1924 examples, 0 failures.

## Open questions

1. Problem 1: keep the shared condition, which also changes the Reports view counts, or limit the change to support chats? Recommended: keep it.
2. Problem 1: the link filter of `queryUnreadGroupItems` changes to `has_link = 1`, the condition of the list. Keep it here? Recommended: keep it.
3. Problem 2: an open "Chats with members" pane keeps the old badges until it is reopened. Fix in the clients separately? Recommended: separately.
4. Problem 3: a plain removal or a member leaving deletes the reports filed by that member with no event. Fix separately? Recommended: separately.

## Order

One pull request; the three changes touch different functions of `Store/Messages.hs`.
