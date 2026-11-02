# Full group link of a group being joined

## Problem

A user joins a group via its short link while the host is offline. The user then opens the group's full link (the link for old clients). The app shows "Connect via link?" and offers to send a second join request. It should show "Group already exists!".

## Cause

- `createPreparedGroup` creates the user's membership with status `GSMemUnknown`.
- The status stays `GSMemUnknown` until the host accepts the request.
- `APIConnectPreparedGroup` sets `groups.via_group_link_uri_hash`.
- `getGroupInfoByGroupLinkHash` excludes memberships with status `GSMemUnknown`, so `groupJoinRequestPlan` does not find the group.
- `groupJoinRequestPlan` then finds the host member's connection (`RcvGroupMsgConnection`) and throws an error.
- `connectPlan` returns the error as `CPError`, and the apps offer to connect again.

## Fix

`getGroupInfoByGroupLinkHash` excludes only `GSMemRemoved`, `GSMemLeft` and `GSMemGroupDeleted`. `groupPlan` returns `GLPConnectingProhibit` for a `GSMemUnknown` membership.

## Scope

- `src/Simplex/Chat/Store/Groups.hs`: `getGroupInfoByGroupLinkHash`.
- `chat_query_plans.txt`: the changed query text.
- Test `testShortLinkGroupFullLinkPlanHostOffline`.
