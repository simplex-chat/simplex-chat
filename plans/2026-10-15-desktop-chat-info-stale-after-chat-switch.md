# Desktop: chat info shows the previous chat's data after switching chats

Fixes #5135.

## Problem

With chat info open in the right pane on desktop, clicking another chat can leave the pane showing the previous chat's data:
- Contact A's info → "Verify security code" → click contact B → "Verify security code": A's code is shown under "B is not verified", and "Mark verified" fails with "Incorrect security code!".
- Group A's info → "Group preferences" → click group B: B's info shows "1 members" and A's group link.
- Switching between a contact and a group with the info shown: the same, without any page on top.
- Contact A → contact B, or group A → group B, with the info shown: until B's info is reloaded, "Verify security code" opens A's code, and "Group link" opens and shares A's link as B's; "Create group link" for a group that has a link fails with "Error creating group link".
- Info on group A → another chat → back to A before the info has loaded: A's info shows only the user as a member.

## Cause

`ChatView` preloads the contact info, security code, group link and group members before it opens the pane, and the pane's state starts from these preloaded values. `KeyChangeEffect(chatInfo.id)` reloads them, but only when the key changes after the first composition of the pane's branch.

The branch is composed again for another chat in two cases, and then nothing is reloaded:
- A chat switch calls `ModalManager.end.closeModalsExceptFirst()` (`ChatListView.kt`), so a page on top of the pane ("Verify security code", "Group preferences", "Group link") is closed and the pane is composed for the new chat.
- The contact and group cases are separate branches, so a switch between a contact and a group composes the other branch.

When the key does change, the contact branch's state is remembered without a key and the group link starts from the previous group's link, so the previous chat's values are shown until the reload returns.

Every chat switch clears the group members, and `setGroupMembers` stores members only while their group is the open chat. After info on group A → another chat → A, the preload has stored no members, and the pane opens for A, the chat its values were preloaded for, so nothing is reloaded.

## Fix

`preloadedChatId` records the chat the preloaded values belong to. The pane reloads in `LaunchedEffect(chatInfo.id)` when the shown chat is not `preloadedChatId`, or when it is a group whose members are not loaded (`!chatModel.membersLoaded.value`). `preloadedChatId` is cleared while a load runs and set when it completes, so a cancelled load is redone.

Until the reload completes, none of the previous chat's values are shown:
- The contact info, security code and group link start empty for another chat (`remember(chatInfo.id)`); without a code the "Verify security code" row is hidden.
- `linkLoaded`, passed to `GroupChatInfoView` as `groupLinkLoaded`, hides the "Create group link" / "Group link" row, disables the channel "Link" button, and hides a channel owner's channel link rows until the link is loaded.

A link created, changed or deleted on the "Group link" page is stored in the preloaded values only while they belong to the group it was made for.

The first open and Back for the same chat do not reload, as before, unless the group's members were cleared.

## Scope

- `ChatView.kt`: the info pane in `ChatView`.
- `GroupChatInfoView.kt`: the `groupLinkLoaded` parameter of `GroupChatInfoView`, `GroupChatInfoLayout` and `ChannelLinkActionButton`.
- Desktop. Android runs the same code, but its info is a full-screen modal that covers the chat list, and a notification tap closes it.
- iOS is unchanged: its info views load the contact info, security code and group link when they appear, and its `ChatView` loads the group members before it shows the group info.
