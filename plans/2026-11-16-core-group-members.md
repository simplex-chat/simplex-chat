# Groups: members, moderation and support chats

One pull request for five core fixes of group membership, moderation and support chats. Each fix has its own plan and was reviewed in its own pull request; this plan states the problems, the approach of each fix and the decisions open for review.

## Problems

| # | Problem as users meet it | Fix branch | Plan |
|---|---|---|---|
| P1 | After an admin removes a member with their messages, reports about and by that member stay active and the reports count stays high | nd/archive-reports-on-member-removal (#7906) | 2026-09-08-archive-reports-on-member-removal.md |
| P2 | After several members are removed at once, each removed member except one sees a database error, and members removed after them in the batch stay "connected" | nd/fix-batch-remove-own-removal-last (#7989) | 2026-11-02-batch-member-removal-own-removal-last.md |
| P3 | After Clear chat on a group, support chat badges stay for messages that no longer exist | nd/clear-group-chat-reset-support-counters | 2026-10-28-clear-chat-support-counters.md |
| P4 | A member support chat keeps requesting the same empty page while other support chats have newer messages | nd/support-chat-navigation-counts | 2026-10-27-group-nav-info-scope.md |
| P5 | The full link of a group the user is still joining offers a new join request instead of "Group already exists!" | nd/full-group-link-joining-plan (#7990) | 2026-11-02-full-group-link-plan-while-joining.md |

## Approach

- P1: on removal with messages, reports about and by the member are marked deleted in `deleteMessages` and listed in the deletion event, as moderation of one message already does.
- P2: a removed member processes its own removal after the other events of the batch, because its own removal deletes the connection that the batch arrived on.
- P3: Clear chat sets each member's support chat counters to 0 and lowers the group's count of members that require attention; `APIClearChat` returns the group read after the deletions.
- P4: the navigation counts of a chat are limited to the listed chat's scope and content filter, so the next page is requested only when it exists; the link filter of unread items becomes `has_link = 1`.
- P5: the group lookup by link hash includes the membership status "unknown", which a group joined by link keeps until the host answers.

## Decisions for review

1. Order of merge: one pull request (recommended), or one per problem.
2. P4: one shared item-set condition for the three queries (applied), or a smaller fix of the scope only.
3. P1: existing reports of members removed before this change stay active (applied); a migration could archive them.

## Tests

Each fix adds a test to tests/ChatTests/Groups.hs, Names.hs or Profiles.hs that fails on master. CI: the reduced build with `/SimpleX chat client/group tests/` and the names and profiles tests.
