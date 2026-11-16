# Member removal and joining

Two fixes of the group membership flows, combined from #7989 and #7990.

## Problems

1. When an admin removes several members at once, each removed member except one sees "chat db error … FOREIGN KEY constraint failed", and in their copy of the group the members removed after them stay connected.
2. A user joins a group by its short link while the host is offline, then opens the group's full link. The app offers to send a second join request instead of showing "Group already exists!".

## Fixes

1. A member who receives a batch of group events processes the events that remove the member after the other events of the batch (`processAgentMessageConn`, `MSG` of a group connection). Plan: `plans/2026-11-02-batch-member-removal-own-removal-last.md`.
2. `getGroupInfoByGroupLinkHash` finds a group whose membership is still unknown (`GSMemUnknown`), so the link plan of a group being joined is "connecting". Plan: `plans/2026-11-02-full-group-link-plan-while-joining.md`.

## Tests

- 1: `testGroupRemoveMembers` (tests/ChatTests/Groups.hs).
- 2: `testShortLinkGroupFullLinkPlanHostOffline` (tests/ChatTests/Profiles.hs), `testChannelNamePlanWhileJoining` (tests/ChatTests/Names.hs).
- Both ran in CI of #7989 and #7990.

## Open questions

1. Fix 1 changes the order of processing in every received group batch. The alternative is to stop processing the batch after the own removal, which drops the later removals. Recommended: the reorder.
2. Fix 1 leaves removals split across several messages unchanged. Recommended: a separate change if it is seen in use.

## Order

The two fixes share no code; they can be merged in one pull request.
