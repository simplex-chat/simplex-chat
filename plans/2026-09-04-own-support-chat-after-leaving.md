# Own support chat after leaving or being removed from a group

## Problem

The "Chat with admins" row in group info (Android/desktop `GroupChatInfoView.kt`, iOS `GroupChatInfoView.swift`) is shown only while `membership.memberActive`, which is false after the user left, was removed or rejected, or the group was deleted. The core keeps the user's own support chat and its items, and the profile unread count (`getUsersInfo`) includes unread support items. So messages received there before the user stopped being active stay unread:

- iOS: there is no way to open that chat; the profile badge stays until they are cleared (Clear chat, deleting the group, or `/read user` in the chat console).
- Android/desktop: for members below moderator, the "N messages" bar at the top of the group chat opens the support chat while it has unread items, but once they are read the history can no longer be opened.

For a member removed while pending review, the removal notice itself is in that support chat.

## Fix

Show the row whenever the user has a support chat (`membership.supportChat != null`); apply the active-member, group-preference and role checks only to offering to start one. Same condition on both platforms. Sending stays blocked by the existing compose states ("you left", "removed from group", "request to join rejected", "group is deleted").

## Alternatives considered

- Show the row only while there are unread items: it disappears after reading, so the history (and a removal notice) can't be reopened.
- Port the Android/desktop "N messages" bar to iOS: same problem after reading, and larger.
- Core: stop counting these items, or mark read / delete the support chat when the member leaves: wrong layer, needs an Android core lib, and deleting loses the record of why the member was removed.
- Open the support chat automatically for inactive members: a bigger navigation change that still needs this condition.

## Not included

- Whether the core should count an inactive member's support items in the profile badge.

## Testing

Desktop AppImage, master vs this PR, with a CLI owner (support on): the app joins by invitation, the owner messages it in the support chat and removes it — master: no "Chat with admins" row (the top bar opens the chat while it has unread messages); PR: the row with the unread badge, the chat opens read-only, and after the messages are read the row stays and still opens the history. After leaving: master no row, PR the row with the badge. Regression: an active member with support on and no support chat, support off without and with a support chat; a moderator with a support chat; a removed member who never had a support chat. Android (Waydroid), with member admission review, removed while pending: master no row; PR the row with badge 2, opening the chat read-only. iOS: not built here.
